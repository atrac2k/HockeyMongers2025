// Storage state arrays
let draftData = [];
let teamsData = [];
let keptData = [];
let selectedKeepers = [];
let teamRosterSizes = {};   // total roster size per team
let teamStats = {};         // { [team]: { draftedKept, rosterSize, percent } }

// Trigger file loading on page load
window.addEventListener('DOMContentLoaded', autoLoadCSVFiles);
document.getElementById('teamSelect').addEventListener('change', loadTeamRoster);

// 1. Silent File Fetch Engine
async function autoLoadCSVFiles() {
    try {
        const draftResponse = await fetch('Draft.csv');
        if (!draftResponse.ok) throw new Error('Could not find Draft.csv in the folder');
        const draftText = await draftResponse.text();
        draftData = parseCSV(draftText);

        const teamsResponse = await fetch('Teams.csv');
        if (!teamsResponse.ok) throw new Error('Could not find Teams.csv in the folder');
        const teamsText = await teamsResponse.text();
        teamsData = parseCSV(teamsText);

        // Kept.csv is optional — don't block the app if it's missing
        try {
            const keptResponse = await fetch('Kept.csv');
            if (keptResponse.ok) {
                const keptText = await keptResponse.text();
                keptData = parseCSV(keptText);
            }
        } catch (keptError) {
            console.warn('Kept.csv not found or failed to load — skipping keeper-history warnings.', keptError);
        }

        checkReadyState();
    } catch (error) {
        console.error(error);
        alert(`File Error: ${error.message}\n\nMake sure Draft.csv and Teams.csv are placed inside the folder and you are running Live Server!`);
    }
}

// 2. FIXED CSV Text Parsing Engine (Safely extracts headers using index 0)
function parseCSV(text) {
    const cleanText = text.replace(/^\uFEFF/, '').trim();
    const lines = cleanText.split(/\r?\n/);
    if (lines.length === 0 || !lines[0].trim()) return [];

    // FIXED: Safely isolate the first row text string before splitting column names
    const firstLineHeaderString = lines[0];
    const headers = firstLineHeaderString.split(',').map(h => h.trim().replace(/^["']|["']$/g, ''));
    const result = [];

    for (let i = 1; i < lines.length; i++) {
        const line = lines[i].trim();
        if (!line) continue;

        const rowValues = [];
        let insideQuotes = false;
        let currentValue = '';

        for (let j = 0; j < line.length; j++) {
            const char = line[j];
            if (char === '"' || char === "'") {
                insideQuotes = !insideQuotes;
            } else if (char === ',' && !insideQuotes) {
                rowValues.push(currentValue.trim().replace(/^["']|["']$/g, ''));
                currentValue = '';
            } else {
                currentValue += char;
            }
        }
        rowValues.push(currentValue.trim().replace(/^["']|["']$/g, ''));

        const obj = {};
        headers.forEach((header, index) => {
            const cleanHeader = header.trim();
            const rawValue = rowValues[index] ? rowValues[index].trim() : '';

            if (cleanHeader.toLowerCase() === 'player') obj['Player'] = rawValue;
            else if (cleanHeader.toLowerCase() === 'team') obj['Team'] = rawValue;
            else if (cleanHeader.toLowerCase() === 'round') obj['Round'] = rawValue;
            else if (cleanHeader.toLowerCase() === 'position') obj['Position'] = rawValue;
            else obj[cleanHeader] = rawValue;
        });
        result.push(obj);
    }
    return result;
}

// 3. Dropdown Menu Generator
function checkReadyState() {
    if (draftData.length > 0 && teamsData.length > 0) {
        const teamSelect = document.getElementById('teamSelect');
        teamSelect.innerHTML = '<option value="">-- Choose a Team --</option>';

        const uniqueTeams = [...new Set(teamsData.map(item => item.Team))].filter(Boolean).sort();

        uniqueTeams.forEach(team => {
            const opt = document.createElement('option');
            opt.value = team;
            opt.textContent = team;
            teamSelect.appendChild(opt);
        });

        // precompute roster size per team
        teamRosterSizes = {};
        uniqueTeams.forEach(team => {
            teamRosterSizes[team] = teamsData.filter(t => t.Team === team).length;
        });

        // Precompute drafted-kept stats for every team up front (so ranks are accurate immediately)
        precomputeAllTeamStats(uniqueTeams);

        document.getElementById('controlSection').style.display = 'block';
    }
}

// 4. Roster List Generator
function loadTeamRoster() {
    const selectedTeam = document.getElementById('teamSelect').value;
    const tbody = document.getElementById('rosterBody');
    tbody.innerHTML = '';
    selectedKeepers = [];
    document.getElementById('summaryCard').style.display = 'none';

    if (!selectedTeam) {
        updateStatsPanel(selectedTeam); // hides panel
        return;
    }

    const modernRoster = teamsData.filter(t => t.Team === selectedTeam);

    const processRoster = modernRoster.map(rosterPlayer => {
        const historic = draftData.find(d => d.Player && rosterPlayer.Player && d.Player.toLowerCase().trim() === rosterPlayer.Player.toLowerCase().trim());

        let origRound, baseCost;
        let position = (rosterPlayer.Position || (historic && historic.Position) || 'N/A').toUpperCase().trim();
        let draftedByText = '';
        let isUndrafted = true;

        const teamIsMissing = !historic || !historic.Team || historic.Team.trim().toUpperCase() === 'NA';

        if (historic && historic.Round !== "" && !teamIsMissing) {
            origRound = parseInt(historic.Round, 10) || 17;
            isUndrafted = false;
            baseCost = origRound - 1;
            draftedByText = historic.Team;
        } else {
            origRound = 17;
            baseCost = 17;
            draftedByText = 'Free Agent';
        }

        return {
            name: rosterPlayer.Player,
            currentTeam: rosterPlayer.Team,
            position: position,
            origRound: origRound,
            baseCost: baseCost,
            draftedByText: draftedByText,
            isUndrafted: isUndrafted
        };
    });

    // Count how many players were drafted by this same team (i.e. "Drafted By" == selected team)
    const draftedByOwnTeamCount = processRoster.filter(
        p => p.draftedByText.toLowerCase() === selectedTeam.toLowerCase()
    ).length;
    const totalPlayers = processRoster.length;
    const percent = totalPlayers > 0 ? (draftedByOwnTeamCount / totalPlayers * 100) : 0;

    teamStats[selectedTeam] = { draftedKept: draftedByOwnTeamCount, rosterSize: totalPlayers, percent };


    // Positional Sorting Rule Matrix: G -> D -> LW/RW -> C
    const positionOrder = { 'G': 1, 'D': 2, 'LD': 2, 'RD': 2, 'LW': 3, 'RW': 3, 'W': 3, 'C': 4 };

    processRoster.sort((a, b) => {
        const orderA = positionOrder[a.position] || 5;
        const orderB = positionOrder[b.position] || 5;
        if (orderA !== orderB) return orderA - orderB;
        return a.name.localeCompare(b.name);
    });

    // Assign each player a group number so LW/RW (and LD/RD) count as the same group for boundary lines
    processRoster.forEach(player => {
        player.positionGroup = positionOrder[player.position] || 5;
    });

    let previousPositionGroup = null;

    processRoster.forEach(player => {
        let draftedByClass = '';
        if (!player.isUndrafted && player.draftedByText.toLowerCase() !== player.currentTeam.toLowerCase()) {
            draftedByClass = 'class="traded-player"';
        } else if (player.isUndrafted) {
            draftedByClass = 'class="undrafted-player"';
        }

        const isBanned = (player.origRound === 1);
        const tr = document.createElement('tr');
        if (isBanned) tr.classList.add('disabled-row');

        // Mark the first row of a new position group (skip the very first row overall)
        if (previousPositionGroup !== null && player.positionGroup !== previousPositionGroup) {
            tr.classList.add('position-boundary');
        }
        previousPositionGroup = player.positionGroup;

        tr.innerHTML = `
            <td>
                <input type="checkbox" class="keeper-cb"
                       data-player="${escapeHtml(player.name)}"
                       data-team="${escapeHtml(player.currentTeam)}"
                       data-orig="${player.origRound}"
                       data-base="${player.baseCost}"
                       ${isBanned ? 'disabled' : ''}>
            </td>
            <td><strong>${escapeHtml(player.name)}</strong></td>
            <td><span class="badge" style="background:#7f8c8d;">${escapeHtml(player.position)}</span></td>
            <td>${player.isUndrafted ? '<span class="badge undrafted">Undrafted</span>' : 'Round ' + player.origRound}</td>
            <td ${draftedByClass}>${escapeHtml(player.draftedByText)}</td>
            <td>${isBanned ? '<span class="badge banned">Ineligible (1st Rounder)</span>' : 'Round ' + player.baseCost}</td>
        `;
        tbody.appendChild(tr);
    });

    const checkboxes = document.querySelectorAll('.keeper-cb');
    checkboxes.forEach(cb => cb.addEventListener('change', calculateKeeperPicks));

    updateStatsPanel(selectedTeam); // show current/stored stats for this team
}

// 5. Keeper Limitation & Tie-Breaker Logic Engine
function calculateKeeperPicks() {
    const checkboxes = document.querySelectorAll('.keeper-cb');
    selectedKeepers = [];

    checkboxes.forEach(cb => {
        if (cb.checked) {
            selectedKeepers.push({
                name: cb.dataset.player,
                teamName: cb.dataset.team,
                origRound: parseInt(cb.dataset.orig, 10),
                finalCost: parseInt(cb.dataset.base, 10),
                isUndrafted: (parseInt(cb.dataset.orig, 10) === 17 && parseInt(cb.dataset.base, 10) === 17)
            });
        }
    });

    if (selectedKeepers.length >= 4) {
        checkboxes.forEach(cb => {
            if (!cb.checked) cb.disabled = true;
        });
    } else {
        checkboxes.forEach(cb => {
            if (parseInt(cb.dataset.orig, 10) !== 1) cb.disabled = false;
        });
    }

    selectedKeepers.sort((a, b) => b.finalCost - a.finalCost);

    let conflictsExist = true;
    while (conflictsExist) {
        conflictsExist = false;
        for (let i = 0; i < selectedKeepers.length; i++) {
            for (let j = i + 1; j < selectedKeepers.length; j++) {
                if (selectedKeepers[i].finalCost === selectedKeepers[j].finalCost) {
                    selectedKeepers[j].finalCost -= 1;
                    conflictsExist = true;
                }
            }
        }
    }

    displaySummary();
}

// 6. Output Panel Renderer
function displaySummary() {
    const card = document.getElementById('summaryCard');
    const list = document.getElementById('summaryList');
    list.innerHTML = '';

    if (selectedKeepers.length === 0) {
        card.style.display = 'none';
        return;
    }

    card.style.display = 'block';
    const displayList = [...selectedKeepers].sort((a, b) => a.finalCost - b.finalCost);

    let anyKeptLastSeason = false;

    displayList.forEach(k => {
        const li = document.createElement('li');
        let originalBaseCost = k.isUndrafted ? 17 : (k.origRound - 1);
        const wasBumped = k.finalCost < originalBaseCost;
        const tieBadge = wasBumped ? ` <span class="badge tie-breaker">Tie-Breaker Active (-1 Round)</span>` : '';

        const keptLastSeason = wasKeptLastSeason(k.name);
        if (keptLastSeason) anyKeptLastSeason = true;
        const threeSeasonBadge = keptLastSeason ? ` <span class="badge three-season-warning">Three Season Warning</span>` : '';

        li.innerHTML = `
            <span><strong>${escapeHtml(k.name)}</strong> ${tieBadge}${threeSeasonBadge}</span>
            <span>Forfeits Pick: <strong>Round ${k.finalCost <= 0 ? '1 (Over Limit)' : k.finalCost}</strong></span>
        `;
        list.appendChild(li);
    });

    // Show the explanation once, only if at least one keeper triggered the warning
    let existingNote = document.getElementById('threeSeasonNote');
    if (existingNote) existingNote.remove();

    if (anyKeptLastSeason) {
        const note = document.createElement('div');
        note.id = 'threeSeasonNote';
        note.className = 'three-season-explanation';
        note.textContent = 'This player was kept last season, he will return to free agency at the end of the season';
        card.appendChild(note);
    }
}

// 7. Team Stats Panel Renderer
function updateStatsPanel(selectedTeam) {
    const panel = document.getElementById('teamStatsPanel');
    if (!selectedTeam) {
        panel.style.display = 'none';
        return;
    }
    panel.style.display = 'flex';

    const stats = teamStats[selectedTeam] || {
        draftedKept: 0,
        rosterSize: teamRosterSizes[selectedTeam] || 0,
        percent: 0
    };

    document.getElementById('statDraftedKept').textContent = stats.draftedKept;
    document.getElementById('statPercentTeam').textContent = stats.percent.toFixed(1) + '%';

    // Rank all teams by % of roster kept (unvisited teams = 0%)
        const allTeams = Object.keys(teamRosterSizes);
        const ranked = allTeams
            .map(team => ({ team, percent: teamStats[team] ? teamStats[team].percent : 0 }))
            .sort((a, b) => b.percent - a.percent);

        const selectedPercent = teamStats[selectedTeam] ? teamStats[selectedTeam].percent : 0;
        const rank = ranked.findIndex(r => r.team === selectedTeam) + 1;

        // Count how many teams share this exact percent (a tie group)
        const tiedCount = ranked.filter(r => r.percent === selectedPercent).length;

        let tieLabel = '';
        if (tiedCount === 2) tieLabel = ' <span class="badge undrafted">2-Way Tie</span>';
        else if (tiedCount === 3) tieLabel = ' <span class="badge undrafted">3-Way Tie</span>';
        else if (tiedCount > 3) tieLabel = ` <span class="badge undrafted">${tiedCount}-Way Tie</span>`;

        document.getElementById('statRank').innerHTML = `${rank} of ${allTeams.length}${tieLabel}`;
}

function escapeHtml(str) {
    if (!str) return '';
    return str
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

// Returns true if this player was kept in the season immediately prior to the current one
function wasKeptLastSeason(playerName) {
    const currentSeason = '26/27';
    const lastSeason = '25/26';
    return keptData.some(k =>
        k.Player &&
        k.Player.toLowerCase().trim() === playerName.toLowerCase().trim() &&
        k.Season && k.Season.trim() === lastSeason
    );
}

function normalizeName(name) {
    return (name || '')
        .toLowerCase()
        .trim()
        .replace(/[.'-]/g, '')          // strip periods, apostrophes, hyphens
        .replace(/\s+jr$|\s+sr$|\s+ii$|\s+iii$/i, '') // strip common suffixes
        .replace(/\s+/g, ' ');
}

// Precompute "Drafted Players Kept" / % for every team, so ranking is accurate before any team is clicked
function precomputeAllTeamStats(uniqueTeams) {
    uniqueTeams.forEach(team => {
        const roster = teamsData.filter(t => t.Team === team);

        const draftedByOwnTeamCount = roster.filter(rosterPlayer => {
            const historic = draftData.find(d => d.Player && rosterPlayer.Player && d.Player.toLowerCase().trim() === rosterPlayer.Player.toLowerCase().trim());
            const teamIsMissing = !historic || !historic.Team || historic.Team.trim().toUpperCase() === 'NA';
            if (!historic || historic.Round === "" || teamIsMissing) return false;
            return historic.Team.toLowerCase() === team.toLowerCase();
        }).length;

        const totalPlayers = roster.length;
        const percent = totalPlayers > 0 ? (draftedByOwnTeamCount / totalPlayers * 100) : 0;

        teamStats[team] = { draftedKept: draftedByOwnTeamCount, rosterSize: totalPlayers, percent };
    });
}
