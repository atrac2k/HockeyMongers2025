// ==================== CONFIG ====================
const TEAMS_IN_LEAGUE = 12;
const SNAKE_DRAFT = false;       // linear draft: same slot order every round
const ADP_GRADED_ROUNDS = 17;    // all rounds count toward the ADP grade
const MISSING_ADP = 152;         // no-ADP players count as going just past the ranked list (never counted as a steal)
const PICK_DIFF_CAP = 40;        // one wild pick can't swing a team's grade more than this
const WEIGHT_PROJ = 0.5, WEIGHT_ADP = 0.5;

// Playoff-odds simulation: no schedule/standings data exists yet, so this is a
// simulation of final standings using each team's total ProjPoints as its
// "talent level" plus random season variance. Adjust freely once real
// league settings (playoff spots, season length) are known.
const PLAYOFF_SPOTS = 6;         // assumed top 6 of 12 make the playoffs
const PLAYOFF_SD_PCT = 0.13;     // assumed season-to-season luck/variance, as % of a team's ProjPoints
const PLAYOFF_SIM_COUNT = 8000;

// Position buckets for the radar chart, keyed off the FIRST listed position
// (e.g. "C, LW" -> C, "LW, RW" -> WING, "RW" -> WING)
const POS_BUCKETS = ['C', 'WING', 'D', 'G'];
const POS_LABELS = { C: 'C', WING: 'LW/RW', D: 'D', G: 'G' };
function posBucket(pos) {
    const first = (pos || '').split(',')[0].trim().toUpperCase();
    if (first === 'C') return 'C';
    if (first === 'LW' || first === 'RW' || first === 'W') return 'WING';
    if (first === 'D') return 'D';
    if (first === 'G') return 'G';
    return null;
}

// Sortable leaderboard column definitions
const COLUMNS = {
    team:     { get: t => t.team,        type: 'string', dir: 'asc' },
    overall:  { get: t => t.overallZ,    type: 'number', dir: 'desc' },
    roster:   { get: t => t.projTotalZ,  type: 'number', dir: 'desc' },
    projTotal:{ get: t => t.projTotal,   type: 'number', dir: 'desc' },
    adp:      { get: t => t.adpScoreZ,   type: 'number', dir: 'desc' },
    adpAvg:   { get: t => t.adpScore,    type: 'number', dir: 'desc' },
    playoff:  { get: t => t.playoffPct,  type: 'number', dir: 'desc' }
};
let sortState = { key: 'overall', dir: 'desc' };

// Hand-written draft recaps, keyed by FantTeam exactly as it appears in the CSV.
// Letter grades from the original write-ups were intentionally left out here —
// this page's own Roster/ADP/Overall grades are shown separately.
const TEAM_WRITEUPS = {
    "ReadyToBustSomeAss!?": {
        pick: 6,
        keepers: "Seider, Necas, Lane Hutson, Schmaltz.",
        body: "This is the best fit for the league's scoring. The keeper group was excellent: Seider is a hits-and-blocks monster, Necas carries a 99-point projection from NHL.com, and Hutson is an elite power-play defenseman. The draft built on that well. Pastrnak and Brady Tkachuk are elite shot generators, Tkachuk adds hits and PIM, Swayman and Askarov give two real starters, and Konecny, Cuylle and Trouba add physical stats. Cuylle in Round 12 and Jarvis in Round 15 were genuine values. The only knocks are a crowded center group and just three defensemen."
    },
    "Jabroni Zamboni goes Tkachoook": {
        pick: 5,
        keepers: "Boldy, Guenther, Gauthier, Marchenko.",
        body: "The keeper group was modest, so this roster was built mostly on draft day, and it was the best draft in the league. McDavid at five was the top value of Round 1. Forsberg, DeBrincat, Reinhart and Tippett followed, giving them the deepest group of shot-producing wingers. Weegar, Nurse and Nikishin supply hits and blocks, and Morrissey adds points from the blue line. Goaltending holds them back: Dostal is the only secure starter, and Vladar has already picked up an upper-body injury."
    },
    "The Mighty Cucks": {
        pick: 1,
        keepers: "Batherson, Suzuki, Zibanejad, Carlsson.",
        body: "MacKinnon is the best player in this format because of his shot volume, and Bouchard and Matthew Tkachuk complete an elite drafted top three. They committed properly in net with Greaves, Wolf and Ullmark, all of whom have starter workloads. The keepers add solid depth, especially Suzuki, whom NHL.com projects for 98 points. Martone in Round 5 was a real pick and a reach for this season, and the defense beyond Bouchard is average."
    },
    "Baby Shark": {
        pick: 7,
        keepers: "Oettinger, Chychrun, Slafkovsky, Celebrini.",
        body: "Keeping Celebrini at a Round 9 cost is the most valuable single keeper in the league, since NHL.com projects him fourth among all forwards at 122 points. Draisaitl and Rantanen were strong draft-day picks, and Oettinger is a reliable workhorse in net. The rest of the draft was less efficient. LaCombe in Round 5 and Harley in Round 6 were early for defensemen who don't hit or block much, and Murashov is a risky second goalie."
    },
    "The Idaho Idiots": {
        pick: 8,
        keepers: "Robertson, Will Smith, Knight, Bussi.",
        body: "Makar, Panarin and Heiskanen join the kept Robertson to form a strong core, and keeping Knight and Bussi gives them real goalie depth. Hellebuyck in Round 4 carries risk: he posted an .895 save percentage in 57 games last season and has publicly asked Winnipeg to trade him. Ovechkin adds age risk and is already day-to-day with a lower-body injury. The drafted forwards also skew heavily toward centers."
    },
    "A Squad": {
        pick: 4,
        keepers: "Stutzle, Logan Thompson.",
        body: "With only two keepers, this roster was mostly built on draft day. Pairing Sorokin in Round 2 with the kept Thompson was one of the smartest moves in the draft, since two workhorse starters are extremely valuable under this goalie scoring. Kucherov, Bratt, Thomas, Point and Kyrou round out a good forward group. The weakness is a defense built for points rather than hits and blocks. Fox, Carlson and Hamilton contribute little physically, and Samuelsson, their most physical defenseman, went down with an undisclosed injury in a preseason game."
    },
    "The Puck Tuahs": {
        pick: 9,
        keepers: "Tage Thompson, Vejmelka.",
        body: "Kaprizov, Johnston and Hyman are strong sources of shots and goals, and Shesterkin plus Vejmelka give them volume in net, with Hart as depth. Raymond, Cooley and Byfield add good forward depth. Waiting until Round 9 to draft a defenseman left them with the thinnest blue line in the league."
    },
    "Playoff Auston Matthews": {
        pick: 12,
        keepers: "Dahlin, Saros, Werenski, Wallstedt.",
        body: "The keepers carry this roster, especially Werenski at a Round 8 cost. Together with the drafted Karlsson and Theodore, they form the best defense corps in the league. Matthews arrived at camp healthy, and Barkov is back after missing all of 2025-26 following ACL and MCL surgery. The wings are thin, Saros is the only established starter, and the draft depends on both stars staying healthy."
    },
    "Not sure": {
        pick: 2,
        keepers: "Horvat, Sherwood, Malkin.",
        body: "Quinn Hughes at second overall was a reach in this format, since his shot and hit totals trail the forwards taken right after him. Bedard in Round 9 was a genuine draft pick and the best late-round value anyone found on draft day, and Connor and Nylander are strong shooters. The defense is deep, and the kept Sherwood is a hits machine. Lankinen, Daccord and Bobrovsky give volume without a true anchor in net. Malkin is a useful keeper after scoring 61 points in 56 games last season."
    },
    "David's Definitive Team": {
        pick: 11,
        keepers: "Tom Wilson, Snuggerud, Raddysh, Cole Hutson.",
        body: "Vasilevskiy in Round 1 is defensible under this scoring, and Guentzel stacks Tampa with him. Crosby in Round 4 was the best value of their draft, and the kept Wilson is one of the best fits for this format because of his hits and PIM. Wedgewood shares Colorado's crease with Blackwood, and the defense is among the weakest in the league. Drafting three prospects on top of two prospect keepers tilts this roster toward future seasons."
    },
    "JustPuckingAround": {
        pick: 10,
        keepers: "Caufield, Faber, Eklund, Sennecke.",
        body: "Eichel and Marner are both Vegas playmakers, which gives them correlated, assist-heavy production with fewer shots and hits than other options at those slots. Markstrom in Round 3 went ahead of Hellebuyck and carries age risk, and Dobes and Hofer are shaky behind him. McKenna in Round 6 is a long-term play as Toronto's newest first overall pick. The kept Caufield is their best value, and Faber and K'Andre Miller add blocks and hits."
    },
    "Car! Game On!": {
        pick: 3,
        keepers: "Larkin, Schaefer, Gibson.",
        body: "Jack Hughes at third overall passed on McDavid, Kucherov and Draisaitl for a player with a lengthy injury history. Keeping Gibson explains part of the wait on goalies, but adding only Blackwood in Round 10 leaves the weakest crease in the league, the biggest hole on any roster under this scoring. The drafted wings are strong in shots and hits, and the kept Schaefer is a bright spot on defense. Larkin also requested a trade from Detroit this summer."
    }
};

let rows = [], results = [];
let radarChart = null, lineChart = null;

window.addEventListener('DOMContentLoaded', async () => {
    try {
        const res = await fetch('2026DraftResults.csv');
        if (!res.ok) throw new Error('Could not load 2026DraftResults.csv');
        rows = parseCSV(await res.text()).map(cleanRow);
        results = gradeTeams(rows);
        computePositionTotals(results);
        computePlayoffOdds(results);
        renderTable();
        renderExtremes();
    } catch (e) {
        document.getElementById('gradeBody').innerHTML = `<tr><td colspan="8">${e.message}</td></tr>`;
    }

    document.querySelectorAll('#gradeTable th[data-key]').forEach(th => {
        th.addEventListener('click', () => {
            const key = th.dataset.key;
            if (sortState.key === key) sortState.dir = sortState.dir === 'asc' ? 'desc' : 'asc';
            else sortState = { key, dir: COLUMNS[key].dir };
            renderTable();
        });
    });

    document.getElementById('modalClose').addEventListener('click', closeModal);
    document.getElementById('modalOverlay').addEventListener('click', e => {
        if (e.target.id === 'modalOverlay') closeModal();
    });
    document.addEventListener('keydown', e => { if (e.key === 'Escape') closeModal(); });
});

// CSV parser that respects quoted commas
function parseCSV(text) {
    const lines = text.replace(/^\uFEFF/, '').trim().split(/\r?\n/);
    const headers = splitLine(lines[0]);
    return lines.slice(1).filter(l => l.trim()).map(l => {
        const v = splitLine(l), o = {};
        headers.forEach((h, i) => o[h] = (v[i] || '').trim());
        return o;
    });
}
function splitLine(line) {
    const out = []; let cur = '', q = false;
    for (const ch of line) {
        if (ch === '"') q = !q;
        else if (ch === ',' && !q) { out.push(cur); cur = ''; }
        else cur += ch;
    }
    out.push(cur);
    return out;
}

function cleanRow(r) {
    const num = s => (s === '' || s === '-' ? NaN : parseFloat(s));
    const round = num(r.Round), slot = num(r.Pick);
    let overall = NaN;
    if (!isNaN(round) && !isNaN(slot)) {
        const s = SNAKE_DRAFT && round % 2 === 0 ? TEAMS_IN_LEAGUE + 1 - slot : slot;
        overall = (round - 1) * TEAMS_IN_LEAGUE + s;
    }
    return {
        name: r.Name, pos: r.Pos, bucket: posBucket(r.Pos), nhl: r.Team, team: r.FantTeam,
        round, overall, keeper: r.Keeper.toLowerCase() === 'k',
        proj: num(r.ProjPoints) || 0, adp: num(r.ADP)
    };
}

function gradeTeams(all) {
    const teams = {};
    all.filter(p => p.team).forEach(p => (teams[p.team] = teams[p.team] || []).push(p));

    // Shared ADP-diff math for any player (real pick or keeper)
    const withAdpDiff = p => {
        const unranked = isNaN(p.adp);
        const adp = unranked ? Math.max(MISSING_ADP, p.overall) : p.adp;
        const raw = p.overall - adp;             // + = went later than ADP, - = went earlier
        return { ...p, unranked, diff: Math.max(-PICK_DIFF_CAP, Math.min(PICK_DIFF_CAP, raw)), rawDiff: raw };
    };

    const out = Object.entries(teams).map(([team, players]) => {
        const projTotal = players.reduce((s, p) => s + p.proj, 0);

        // Every graded-round entry with a valid overall pick, keepers included —
        // used only for the round-by-round chart, never for grading.
        const chartEntries = players
            .filter(p => !isNaN(p.overall) && p.round <= ADP_GRADED_ROUNDS)
            .map(withAdpDiff)
            .sort((a, b) => a.round - b.round);

        // ADP: only real draft picks (no keepers) — this is what's actually graded
        const picks = chartEntries.filter(p => !p.keeper);
        const adpScore = picks.length ? picks.reduce((s, p) => s + p.diff, 0) / picks.length : 0;
        const byRawDesc = [...picks].sort((a, b) => b.rawDiff - a.rawDiff);
        return {
            team, players, projTotal, adpScore, picks, chartEntries,
            bestPick: byRawDesc[0], worstPick: byRawDesc[byRawDesc.length - 1],
            top5: byRawDesc.slice(0, 5),
            bottom5: [...byRawDesc].reverse().slice(0, 5)
        };
    });

    const z = (key) => {
        const v = out.map(t => t[key]), m = v.reduce((a, b) => a + b, 0) / v.length;
        const sd = Math.sqrt(v.reduce((a, b) => a + (b - m) ** 2, 0) / v.length) || 1;
        out.forEach(t => t[key + 'Z'] = (t[key] - m) / sd);
    };
    z('projTotal'); z('adpScore');
    out.forEach(t => {
        t.overallZ = WEIGHT_PROJ * t.projTotalZ + WEIGHT_ADP * t.adpScoreZ;
        t.projGrade = letter(t.projTotalZ);
        t.adpGrade = letter(t.adpScoreZ);
        t.overallGrade = letter(t.overallZ);
    });
    return out;
}

// Total ProjPoints per position bucket, per team, plus the league-average team for comparison
function computePositionTotals(teamResults) {
    teamResults.forEach(t => {
        const totals = { C: 0, WING: 0, D: 0, G: 0 };
        t.players.forEach(p => { if (p.bucket) totals[p.bucket] += p.proj; });
        t.posTotals = totals;
    });
    const league = { C: 0, WING: 0, D: 0, G: 0 };
    POS_BUCKETS.forEach(b => {
        league[b] = teamResults.reduce((s, t) => s + t.posTotals[b], 0) / teamResults.length;
    });
    window.__leagueAvgPos = league;
}

// Monte Carlo estimate of playoff odds: each simulated season draws a final
// score for every team from Normal(ProjPoints, PLAYOFF_SD_PCT * ProjPoints),
// then the top PLAYOFF_SPOTS teams "make it." No real schedule exists, so this
// is a talent-plus-variance proxy, not an actual standings projection.
function computePlayoffOdds(teamResults) {
    const counts = new Array(teamResults.length).fill(0);
    for (let s = 0; s < PLAYOFF_SIM_COUNT; s++) {
        const scores = teamResults.map(t => t.projTotal + gaussianRandom() * t.projTotal * PLAYOFF_SD_PCT);
        const order = scores.map((v, i) => [v, i]).sort((a, b) => b[0] - a[0]);
        for (let k = 0; k < PLAYOFF_SPOTS && k < order.length; k++) counts[order[k][1]]++;
    }
    teamResults.forEach((t, i) => { t.playoffPct = 100 * counts[i] / PLAYOFF_SIM_COUNT; });
}
function gaussianRandom() {
    let u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

function letter(z) {
    const cuts = [[1.5,'A+'],[1.0,'A'],[0.6,'A-'],[0.3,'B+'],[0,'B'],[-0.3,'B-'],[-0.6,'C+'],[-1.0,'C'],[-1.5,'C-']];
    for (const [c, g] of cuts) if (z >= c) return g;
    return 'D';
}

function esc(s) { return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;'); }
function gradeClass(g) { return 'grade grade-' + g[0].toLowerCase(); }
function signed(n) { return (n > 0 ? '+' : '') + n.toFixed(1); }

// ==================== MAIN TABLE (sortable) ====================
function renderTable() {
    const col = COLUMNS[sortState.key];
    const sorted = [...results].sort((a, b) => {
        const av = col.get(a), bv = col.get(b);
        const cmp = col.type === 'string' ? String(av).localeCompare(String(bv)) : av - bv;
        return sortState.dir === 'asc' ? cmp : -cmp;
    });
    document.getElementById('gradeBody').innerHTML = sorted.map((t, i) => `
        <tr class="team-row" data-team="${esc(t.team)}" tabindex="0">
            <td>${i + 1}</td>
            <td><strong>${esc(t.team)}</strong></td>
            <td><span class="${gradeClass(t.overallGrade)}">${t.overallGrade}</span></td>
            <td><span class="${gradeClass(t.projGrade)}">${t.projGrade}</span></td>
            <td>${Math.round(t.projTotal).toLocaleString()}</td>
            <td><span class="${gradeClass(t.adpGrade)}">${t.adpGrade}</span></td>
            <td>${signed(t.adpScore)}</td>
            <td>${t.playoffPct.toFixed(0)}%</td>
        </tr>`).join('');
    document.querySelectorAll('.team-row').forEach(tr => {
        tr.addEventListener('click', () => openModal(tr.dataset.team));
        tr.addEventListener('keydown', e => { if (e.key === 'Enter') openModal(tr.dataset.team); });
    });
    document.querySelectorAll('#gradeTable th[data-key]').forEach(th => {
        th.classList.remove('sorted-asc', 'sorted-desc');
        if (th.dataset.key === sortState.key) th.classList.add(sortState.dir === 'asc' ? 'sorted-asc' : 'sorted-desc');
    });
}

// ==================== STEALS & STINKERS (league-wide) ====================
function renderExtremes() {
    const all = results.flatMap(t => t.picks);           // keepers already excluded
    const steals = all.filter(p => !p.unranked).sort((a, b) => b.rawDiff - a.rawDiff).slice(0, 10);
    const stinkers = [...all].sort((a, b) => a.rawDiff - b.rawDiff).slice(0, 10);
    const rowsHtml = list => list.map((p, i) => `
        <tr>
            <td>${i + 1}</td>
            <td><strong>${esc(p.name)}</strong> <span class="muted">${esc(p.pos)}</span></td>
            <td>${esc(p.team)}</td>
            <td>Rd ${p.round} (#${p.overall})</td>
            <td>${p.unranked ? 'n/a' : p.adp}</td>
            <td><strong>${signed(p.rawDiff)}</strong></td>
        </tr>`).join('');
    document.getElementById('stealsBody').innerHTML = rowsHtml(steals);
    document.getElementById('stinkersBody').innerHTML = rowsHtml(stinkers);
}

// ==================== TEAM MODAL ====================
function openModal(teamName) {
    const t = results.find(r => r.team === teamName);
    if (!t) return;

    document.getElementById('modalTitle').textContent = t.team;
    document.getElementById('modalSummary').innerHTML = buildSummary(t);
    document.getElementById('top5Body').innerHTML = pickRows(t.top5);
    document.getElementById('bottom5Body').innerHTML = pickRows(t.bottom5);

    const overlay = document.getElementById('modalOverlay');
    overlay.style.display = 'flex';
    document.body.classList.add('modal-open');

    if (typeof Chart === 'undefined') {
        document.querySelectorAll('.chart-canvas-wrap').forEach(el => {
            el.innerHTML = '<p class="muted">Charts failed to load (Chart.js didn\u2019t load from the CDN). Check your connection and reload.</p>';
        });
        return;
    }

    // The modal was just made visible, so the canvases have no real layout size
    // yet. Force a reflow and wait a frame before letting Chart.js measure them,
    // or it renders at 0x0 and appears blank.
    void overlay.offsetHeight;
    requestAnimationFrame(() => {
        requestAnimationFrame(() => {
            drawRadar(t);
            drawAdpLine(t);
        });
    });
}

function closeModal() {
    document.getElementById('modalOverlay').style.display = 'none';
    document.body.classList.remove('modal-open');
    if (radarChart) { radarChart.destroy(); radarChart = null; }
    if (lineChart) { lineChart.destroy(); lineChart = null; }
}

function pickRows(list) {
    if (!list.length) return '<tr><td colspan="6">No graded picks.</td></tr>';
    return list.map(p => `
        <tr>
            <td><strong>${esc(p.name)}</strong></td>
            <td>${esc(p.pos)}</td>
            <td>${p.overall}</td>
            <td>${p.unranked ? 'n/a' : p.adp}</td>
            <td>${p.proj.toFixed(1)}</td>
            <td><strong>${signed(p.rawDiff)}</strong></td>
        </tr>`).join('');
}

// ---- Radar: team ProjPoints by position bucket vs league average ----
function drawRadar(t) {
    const canvas = document.getElementById('radarCanvas');
    const ctx = canvas.getContext('2d');
    if (radarChart) radarChart.destroy();
    const league = window.__leagueAvgPos;
    radarChart = new Chart(ctx, {
        type: 'radar',
        data: {
            labels: POS_BUCKETS.map(b => POS_LABELS[b]),
            datasets: [
                {
                    label: t.team,
                    data: POS_BUCKETS.map(b => Math.round(t.posTotals[b])),
                    backgroundColor: 'rgba(52,152,219,0.25)',
                    borderColor: '#3498db',
                    pointBackgroundColor: '#3498db'
                },
                {
                    label: 'League Average',
                    data: POS_BUCKETS.map(b => Math.round(league[b])),
                    backgroundColor: 'rgba(149,165,166,0.15)',
                    borderColor: '#95a5a6',
                    pointBackgroundColor: '#95a5a6'
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            animation: false,
            scales: { r: { beginAtZero: true, pointLabels: { font: { size: 12 } } } },
            plugins: { legend: { position: 'bottom' } }
        }
    });
}

// ---- Line: pick vs ADP difference by round, for real picks AND keepers ----
// Keepers are shown for context (grey) but never count toward the team's ADP grade.
function drawAdpLine(t) {
    const canvas = document.getElementById('lineCanvas');
    const ctx = canvas.getContext('2d');
    if (lineChart) lineChart.destroy();
    const byRound = {};
    t.chartEntries.forEach(p => { byRound[p.round] = p; }); // one entry per round in a standard draft
    const rounds = Array.from({ length: ADP_GRADED_ROUNDS }, (_, i) => i + 1);
    const data = rounds.map(r => byRound[r] ? byRound[r].rawDiff : null);
    const entries = rounds.map(r => byRound[r] || null);

    const pointColor = e => {
        if (!e) return 'transparent';
        if (e.keeper) return '#95a5a6';
        return e.rawDiff >= 0 ? '#27ae60' : '#e74c3c';
    };
    const segmentColor = c => {
        const e0 = entries[c.p0.parsed.x], e1 = entries[c.p1.parsed.x];
        if ((e0 && e0.keeper) || (e1 && e1.keeper)) return '#bbb';
        if (e0.rawDiff >= 0 && e1.rawDiff >= 0) return '#27ae60';
        if (e0.rawDiff < 0 && e1.rawDiff < 0) return '#e74c3c';
        return '#95a5a6';
    };

    lineChart = new Chart(ctx, {
        type: 'line',
        data: {
            labels: rounds.map(r => 'Rd ' + r),
            datasets: [{
                label: 'Pick vs ADP',
                data,
                spanGaps: true,
                segment: { borderColor: segmentColor, borderDash: c => {
                    const e0 = entries[c.p0.parsed.x], e1 = entries[c.p1.parsed.x];
                    return ((e0 && e0.keeper) || (e1 && e1.keeper)) ? [4, 3] : undefined;
                } },
                pointBackgroundColor: entries.map(pointColor),
                pointBorderColor: entries.map(pointColor),
                pointRadius: entries.map(e => e ? 4 : 0),
                borderColor: '#3498db',
                tension: 0.15,
                fill: false
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            animation: false,
            plugins: {
                legend: { display: false },
                tooltip: {
                    callbacks: {
                        title: items => {
                            const e = entries[items[0].dataIndex];
                            return e ? `${e.name}${e.keeper ? ' (Keeper)' : ''}` : items[0].label;
                        },
                        label: item => {
                            const e = entries[item.dataIndex];
                            if (!e) return 'No pick this round';
                            const tag = e.keeper ? 'Keeper cost' : (e.unranked ? 'No ADP on file' : 'vs ADP');
                            return `${e.rawDiff > 0 ? '+' : ''}${e.rawDiff} ${tag}`;
                        }
                    }
                }
            },
            scales: {
                y: { title: { display: true, text: 'Pick \u2212 ADP (+ = value)' } }
            }
        }
    });
}

// ---- Auto-generated draft summary (with the hand-written recap, if any, up top) ----
function buildSummary(t) {
    const parts = [];
    const w = TEAM_WRITEUPS[t.team];
    if (w) {
        parts.push(`<p class="muted">Drafted from pick ${w.pick}.</p>`);
        parts.push(`<p><strong>Keepers:</strong> ${esc(w.keepers)}</p>`);
        parts.push(`<p>${esc(w.body)}</p>`);
        parts.push(`<hr class="summary-divider"><p class="muted">By the numbers:</p>`);
    }

    const gradeWord = g => ({ 'A+':'elite', A:'excellent', 'A-':'strong', 'B+':'solid', B:'average',
        'B-':'a bit below average', 'C+':'shaky', C:'weak', 'C-':'poor', D:'the worst in the league' }[g] || g);

    const posRank = {};
    POS_BUCKETS.forEach(b => {
        const ranked = [...results].sort((a, c) => c.posTotals[b] - a.posTotals[b]);
        posRank[b] = ranked.findIndex(r => r.team === t.team) + 1;
    });
    const bestPos = POS_BUCKETS.reduce((a, b) => posRank[b] < posRank[a] ? b : a);
    const worstPos = POS_BUCKETS.reduce((a, b) => posRank[b] > posRank[a] ? b : a);

    parts.push(`<p><strong>${esc(t.team)}</strong> put together a roster with a <strong>${t.projGrade}</strong> projected points grade (${gradeWord(t.projGrade)}) and a <strong>${t.adpGrade}</strong> for value relative to ADP (${gradeWord(t.adpGrade)}), for an overall grade of <strong>${t.overallGrade}</strong>.</p>`);
    parts.push(`<p>Their strongest group is <strong>${POS_LABELS[bestPos]}</strong>, ranking #${posRank[bestPos]} in the league at that position, while <strong>${POS_LABELS[worstPos]}</strong> is their weak spot at #${posRank[worstPos]}.</p>`);
    if (t.bestPick) {
        parts.push(`<p>Their best value pick was <strong>${esc(t.bestPick.name)}</strong> in round ${t.bestPick.round} (ADP ${t.bestPick.unranked ? 'n/a' : t.bestPick.adp}, ${signed(t.bestPick.rawDiff)} picks of value)${t.worstPick ? `, while <strong>${esc(t.worstPick.name)}</strong> in round ${t.worstPick.round} was their biggest reach (${signed(t.worstPick.rawDiff)}).` : '.'}</p>`);
    }
    parts.push(`<p>On average they drafted players <strong>${signed(t.adpScore)}</strong> picks ${t.adpScore >= 0 ? 'later' : 'earlier'} than ADP, ${t.adpScore >= 0 ? 'consistently finding value across the board' : 'showing a willingness to reach for targeted players'}. Simulated playoff odds: <strong>${t.playoffPct.toFixed(0)}%</strong>.</p>`);
    return parts.join('');
}
