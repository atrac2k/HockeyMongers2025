// ==================== CONFIG ====================
// Change these each year to move the calendar to the correct draft month/year.
const DRAFT_YEAR = 2026;
const DRAFT_MONTH = 9; // September (1 = Jan ... 12 = Dec)

// ==================== STATE ====================
// availabilityData: { "Team Name": { "2026-09-05": [9,10,11], ... }, ... }
//   - key = date string, value = array of blocked hours (0-23, hour 9 = 9:00-10:00 blocked)
//   - a date with all 24 hours listed = blocked all day
let availTeamsList = [];
let availabilityData = {};

// currentTeamDates: Map<dateStr, Set<hour>> — working copy for whichever team is selected, not yet saved
let currentTeamDates = new Map();

// Which date the hour-picker modal is currently open for
let modalDate = null;
let modalHours = new Set(); // working copy while modal is open

document.addEventListener('DOMContentLoaded', initAvailabilityTab);

async function initAvailabilityTab() {
    document.getElementById('calendarHeading').textContent =
        `Mark the dates in ${monthName(DRAFT_MONTH)} ${DRAFT_YEAR} you CANNOT draft`;

    buildCalendar();
    await loadAvailTeams();
    await loadAvailabilityData();
    renderOverview();
    renderOverviewCalendar();

    document.getElementById('availTeamSelect').addEventListener('change', onAvailTeamChange);
    document.getElementById('saveAvailabilityBtn').addEventListener('click', saveAvailability);

    document.getElementById('markFullDayBtn').addEventListener('click', () => setModalHours(allHours()));
    document.getElementById('clearDayBtn').addEventListener('click', () => setModalHours([]));
    document.getElementById('hourModalDone').addEventListener('click', closeModalAndApply);
    document.getElementById('hourModalCancel').addEventListener('click', closeModalDiscard);
    document.getElementById('hourModal').addEventListener('click', (e) => {
        if (e.target.id === 'hourModal') closeModalDiscard(); // click outside the box
    });
}

// ---- Load team list from Teams.csv (kept independent from script.js on purpose) ----
async function loadAvailTeams() {
    try {
        const res = await fetch('Teams.csv');
        if (!res.ok) throw new Error('Could not load Teams.csv');
        const text = await res.text();
        const rows = parseSimpleCSV(text);
        availTeamsList = [...new Set(rows.map(r => r.Team))].filter(Boolean).sort();

        const select = document.getElementById('availTeamSelect');
        availTeamsList.forEach(team => {
            const opt = document.createElement('option');
            opt.value = team;
            opt.textContent = team;
            select.appendChild(opt);
        });
    } catch (e) {
        console.error(e);
        document.getElementById('availStatus').textContent = 'Error loading team list.';
    }
}

// Lightweight CSV parser (mirrors script.js's logic; kept local so this file has no dependency)
function parseSimpleCSV(text) {
    const clean = text.replace(/^\uFEFF/, '').trim();
    const lines = clean.split(/\r?\n/);
    if (!lines.length || !lines[0].trim()) return [];
    const headers = lines[0].split(',').map(h => h.trim().replace(/^["']|["']$/g, ''));
    const out = [];
    for (let i = 1; i < lines.length; i++) {
        const line = lines[i].trim();
        if (!line) continue;
        const vals = line.split(',').map(v => v.trim().replace(/^["']|["']$/g, ''));
        const obj = {};
        headers.forEach((h, idx) => { obj[h] = vals[idx] || ''; });
        out.push(obj);
    }
    return out;
}

// ---- Load shared availability data written by save_availability.php ----
async function loadAvailabilityData() {
    try {
        const res = await fetch('availability.json?_=' + Date.now()); // cache-bust so everyone sees fresh data
        if (res.ok) {
            const raw = await res.json();
            availabilityData = normalizeAvailabilityData(raw);
        }
    } catch (e) {
        console.warn('No existing availability data found yet.', e);
        availabilityData = {};
    }
}

// Handles older saved data (plain arrays of date strings = full-day only) so nothing breaks
// if this is running against data saved before hour-level picking existed.
function normalizeAvailabilityData(raw) {
    const normalized = {};
    Object.keys(raw || {}).forEach(team => {
        const val = raw[team];
        if (Array.isArray(val)) {
            // legacy format: array of date strings, each meant "unavailable all day"
            const byDate = {};
            val.forEach(dateStr => { byDate[dateStr] = allHours(); });
            normalized[team] = byDate;
        } else if (val && typeof val === 'object') {
            normalized[team] = val;
        } else {
            normalized[team] = {};
        }
    });
    return normalized;
}

function allHours() {
    return Array.from({ length: 24 }, (_, i) => i);
}

// ---- Build the calendar grid for the configured month/year ----
function buildCalendar() {
    const grid = document.getElementById('calendarGrid');
    grid.innerHTML = '';

    const dayNames = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
    dayNames.forEach(d => {
        const head = document.createElement('div');
        head.className = 'calendar-day-header';
        head.textContent = d;
        grid.appendChild(head);
    });

    const firstDay = new Date(DRAFT_YEAR, DRAFT_MONTH - 1, 1);
    const startWeekday = firstDay.getDay();
    const daysInMonth = new Date(DRAFT_YEAR, DRAFT_MONTH, 0).getDate();

    for (let i = 0; i < startWeekday; i++) {
        const blank = document.createElement('div');
        blank.className = 'calendar-cell empty';
        grid.appendChild(blank);
    }

    for (let d = 1; d <= daysInMonth; d++) {
        const dateStr = formatDate(DRAFT_YEAR, DRAFT_MONTH, d);
        const cell = document.createElement('div');
        cell.className = 'calendar-cell';
        cell.dataset.date = dateStr;
        cell.innerHTML = `<span class="cal-num">${d}</span><span class="cal-hours-count"></span>`;

        // Click: toggle this day in/out of the Selected Days list (defaults to Full Day when added)
        cell.addEventListener('click', () => toggleDaySelection(dateStr));

        grid.appendChild(cell);
    }

    refreshAllCellStyles();
}

function formatDate(y, m, d) {
    return `${y}-${String(m).padStart(2, '0')}-${String(d).padStart(2, '0')}`;
}

function monthName(m) {
    const names = ['January','February','March','April','May','June','July','August','September','October','November','December'];
    return names[m - 1] || '';
}

function hourLabel(h) {
    const hh = ((h % 24) + 24) % 24;
    if (hh === 0) return '12am';
    if (hh === 12) return '12pm';
    return hh < 12 ? `${hh}am` : `${hh - 12}pm`;
}

// Groups consecutive blocked hours into readable ranges, e.g. [9,10,11] -> "9am–12pm"
function formatHourRanges(hoursArray) {
    if (!hoursArray || hoursArray.length === 0) return '';
    const sorted = [...hoursArray].sort((a, b) => a - b);
    if (sorted.length === 24) return 'Full Day';

    const ranges = [];
    let start = sorted[0];
    let prev = sorted[0];

    for (let i = 1; i <= sorted.length; i++) {
        const cur = sorted[i];
        if (cur !== prev + 1) {
            ranges.push(`${hourLabel(start)}\u2013${hourLabel(prev + 1)}`);
            start = cur;
        }
        prev = cur;
    }
    return ranges.join(', ');
}

// ---- Click on a calendar cell: toggle it in/out of the Selected Days list ----
function toggleDaySelection(dateStr) {
    const select = document.getElementById('availTeamSelect');
    if (!select.value) {
        alert('Please choose your team first.');
        return;
    }
    const existing = currentTeamDates.get(dateStr);
    if (existing && existing.size > 0) {
        currentTeamDates.delete(dateStr); // already selected -> remove it
    } else {
        currentTeamDates.set(dateStr, new Set(allHours())); // newly selected -> default to Full Day
    }
    refreshCellStyle(dateStr);
    renderSelectedDaysList();
}

// ---- Set a selected day to Full Day (all 24 hours) ----
function setDayFullDay(dateStr) {
    currentTeamDates.set(dateStr, new Set(allHours()));
    refreshCellStyle(dateStr);
    renderSelectedDaysList();
}

// ---- Remove a day entirely from the selection ----
function removeSelectedDay(dateStr) {
    currentTeamDates.delete(dateStr);
    refreshCellStyle(dateStr);
    renderSelectedDaysList();
}

// ---- Render the "Selected Days" list between the calendar and the Save button ----
function renderSelectedDaysList() {
    const listEl = document.getElementById('selectedDaysList');
    const heading = document.getElementById('selectedDaysHeading');
    listEl.innerHTML = '';

    const sortedDates = Array.from(currentTeamDates.keys()).sort();

    if (sortedDates.length === 0) {
        heading.style.display = 'none';
        return;
    }
    heading.style.display = 'block';

    sortedDates.forEach(dateStr => {
        const hours = currentTeamDates.get(dateStr);
        const isFullDay = hours && hours.size === 24;

        const row = document.createElement('div');
        row.className = 'selected-day-row';
        row.innerHTML = `
            <span class="selected-day-date">${formatDateLabel(dateStr)}</span>
            <span class="selected-day-state">${formatHourRanges(Array.from(hours))}</span>
            <span class="selected-day-actions">
                <button type="button" class="day-action-btn full-day-btn${isFullDay ? ' active' : ''}">Full Day</button>
                <button type="button" class="day-action-btn hours-btn">Hours</button>
                <button type="button" class="day-action-btn remove-btn">Remove</button>
            </span>
        `;

        row.querySelector('.full-day-btn').addEventListener('click', () => setDayFullDay(dateStr));
        row.querySelector('.hours-btn').addEventListener('click', () => openHourModal(dateStr));
        row.querySelector('.remove-btn').addEventListener('click', () => removeSelectedDay(dateStr));

        listEl.appendChild(row);
    });
}

function formatDateLabel(dateStr) {
    const d = new Date(dateStr + 'T00:00:00');
    return d.toLocaleDateString(undefined, { weekday: 'short', month: 'short', day: 'numeric' });
}

// ---- Hour picker modal ----
function openHourModal(dateStr) {
    const select = document.getElementById('availTeamSelect');
    if (!select.value) {
        alert('Please choose your team first.');
        return;
    }
    modalDate = dateStr;
    modalHours = new Set(currentTeamDates.get(dateStr) || []);

    document.getElementById('hourModalTitle').textContent = `Unavailable Hours \u2014 ${dateStr}`;
    renderHourGrid();
    document.getElementById('hourModal').style.display = 'flex';
}

function renderHourGrid() {
    const gridEl = document.getElementById('hourGrid');
    gridEl.innerHTML = '';
    for (let h = 0; h < 24; h++) {
        const btn = document.createElement('div');
        btn.className = 'hour-btn' + (modalHours.has(h) ? ' blocked' : '');
        btn.textContent = hourLabel(h);
        btn.addEventListener('click', () => {
            if (modalHours.has(h)) modalHours.delete(h);
            else modalHours.add(h);
            renderHourGrid();
        });
        gridEl.appendChild(btn);
    }
}

function setModalHours(hoursArray) {
    modalHours = new Set(hoursArray);
    renderHourGrid();
}

function closeModalAndApply() {
    if (modalHours.size > 0) {
        currentTeamDates.set(modalDate, new Set(modalHours));
    } else {
        currentTeamDates.delete(modalDate);
    }
    refreshCellStyle(modalDate);
    renderSelectedDaysList();
    document.getElementById('hourModal').style.display = 'none';
    modalDate = null;
}

function closeModalDiscard() {
    document.getElementById('hourModal').style.display = 'none';
    modalDate = null;
}

// ---- Cell styling based on currentTeamDates ----
function refreshCellStyle(dateStr) {
    const cell = document.querySelector(`.calendar-cell[data-date="${dateStr}"]`);
    if (!cell) return;
    const hours = currentTeamDates.get(dateStr);
    const countEl = cell.querySelector('.cal-hours-count');

    cell.classList.remove('unavailable', 'partial');
    if (hours && hours.size === 24) {
        cell.classList.add('unavailable');
        countEl.textContent = '';
    } else if (hours && hours.size > 0) {
        cell.classList.add('partial');
        countEl.textContent = `${hours.size}h`;
    } else {
        countEl.textContent = '';
    }
}

function refreshAllCellStyles() {
    document.querySelectorAll('.calendar-cell:not(.empty)').forEach(cell => {
        refreshCellStyle(cell.dataset.date);
    });
}

// ---- When a different team is selected, load their previously saved dates ----
function onAvailTeamChange() {
    const team = document.getElementById('availTeamSelect').value;
    const teamData = availabilityData[team] || {};
    currentTeamDates = new Map(
        Object.keys(teamData).map(dateStr => [dateStr, new Set(teamData[dateStr])])
    );

    refreshAllCellStyles();
    renderSelectedDaysList();

    document.getElementById('availStatus').textContent = team ? `Editing availability for ${team}` : '';
    document.getElementById('availStatus').classList.remove('loaded');
}

// ---- Save current team's dates to the shared backend ----
async function saveAvailability() {
    const team = document.getElementById('availTeamSelect').value;
    if (!team) {
        alert('Please choose your team first.');
        return;
    }

    const statusEl = document.getElementById('availStatus');
    statusEl.textContent = 'Saving...';
    statusEl.classList.remove('loaded');

    const days = {};
    currentTeamDates.forEach((hourSet, dateStr) => {
        if (hourSet.size > 0) days[dateStr] = Array.from(hourSet).sort((a, b) => a - b);
    });

    try {
        const res = await fetch('save_availability.php', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ team, days })
        });
        const result = await res.json();
        if (!res.ok || result.error) throw new Error(result.error || 'Save failed');

        availabilityData = normalizeAvailabilityData(result.data);
        statusEl.textContent = 'Saved!';
        statusEl.classList.add('loaded');
        renderOverview();
        renderOverviewCalendar();

        setTimeout(() => {
            statusEl.textContent = '';
            statusEl.classList.remove('loaded');
        }, 2500);
    } catch (e) {
        console.error(e);
        statusEl.textContent = 'Error saving \u2014 please try again.';
    }
}

// ---- Render the color-coded overview calendar (green/yellow/red + unavailable count) ----
function renderOverviewCalendar() {
    const grid = document.getElementById('overviewCalendarGrid');
    grid.innerHTML = '';

    const dayNames = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
    dayNames.forEach(d => {
        const head = document.createElement('div');
        head.className = 'calendar-day-header';
        head.textContent = d;
        grid.appendChild(head);
    });

    const firstDay = new Date(DRAFT_YEAR, DRAFT_MONTH - 1, 1);
    const startWeekday = firstDay.getDay();
    const daysInMonth = new Date(DRAFT_YEAR, DRAFT_MONTH, 0).getDate();
    const totalTeams = availTeamsList.length;

    for (let i = 0; i < startWeekday; i++) {
        const blank = document.createElement('div');
        blank.className = 'calendar-cell empty';
        grid.appendChild(blank);
    }

    for (let d = 1; d <= daysInMonth; d++) {
        const dateStr = formatDate(DRAFT_YEAR, DRAFT_MONTH, d);

        const blockedEntries = Object.keys(availabilityData)
            .map(t => ({ team: t, hours: (availabilityData[t] || {})[dateStr] }))
            .filter(e => e.hours && e.hours.length > 0);

        const count = blockedEntries.length;

        let colorClass = 'avail-green';
        if (totalTeams > 0 && count === totalTeams) colorClass = 'avail-red';
        else if (count > 0) colorClass = 'avail-yellow';

        const cell = document.createElement('div');
        cell.className = `calendar-cell overview-cell ${colorClass}`;
        cell.innerHTML = `<span class="cal-num">${d}</span><span class="cal-count">${count}</span>`;

        if (blockedEntries.length) {
            cell.title = blockedEntries
                .map(e => `${e.team} (${formatHourRanges(e.hours)})`)
                .join('\n');
        } else {
            cell.title = 'Everyone available';
        }

        grid.appendChild(cell);
    }
}

// ---- Render the league-wide overview table ----
function renderOverview() {
    const container = document.getElementById('availabilityOverview');
    const bestDayNoteEl = document.getElementById('bestDayNote');
    container.innerHTML = '';

    const daysInMonth = new Date(DRAFT_YEAR, DRAFT_MONTH, 0).getDate();
    const teamsWithData = Object.keys(availabilityData).filter(t => Object.keys(availabilityData[t] || {}).length > 0);

    if (teamsWithData.length === 0) {
        container.innerHTML = '<p class="status">No one has submitted their availability yet.</p>';
        if (bestDayNoteEl) bestDayNoteEl.innerHTML = '';
        return;
    }

    // Gather stats for every day first so the best day can be picked before building rows
    const dayStats = [];
    for (let d = 1; d <= daysInMonth; d++) {
        const dateStr = formatDate(DRAFT_YEAR, DRAFT_MONTH, d);
        const blockedEntries = teamsWithData
            .map(t => ({ team: t, hours: (availabilityData[t] || {})[dateStr] }))
            .filter(e => e.hours && e.hours.length > 0);
        dayStats.push({ day: d, dateStr, blockedEntries, count: blockedEntries.length });
    }

    // Best day = fewest teams with any conflict; ties broken by closeness to the 27th
    let best = null;
    dayStats.forEach(stat => {
        const distance = Math.abs(stat.day - 27);
        if (!best || stat.count < best.count || (stat.count === best.count && distance < best.distance)) {
            best = { ...stat, distance };
        }
    });

    const table = document.createElement('table');
    table.className = 'roster-table';
    const thead = document.createElement('thead');
    thead.innerHTML = '<tr><th>Date</th><th>Teams Unavailable</th><th>Count</th></tr>';
    table.appendChild(thead);
    const tbody = document.createElement('tbody');

    dayStats.forEach(stat => {
        const teamsLabel = stat.blockedEntries.length
            ? stat.blockedEntries.map(e => `${escapeHtmlLocal(e.team)} (${formatHourRanges(e.hours)})`).join('; ')
            : '<span class="badge" style="background:#27ae60;">Everyone free</span>';

        const tr = document.createElement('tr');
        if (stat.dateStr === best.dateStr) tr.classList.add('best-day-row');
        tr.innerHTML = `
            <td>${stat.dateStr}</td>
            <td>${teamsLabel}</td>
            <td>${stat.count}</td>
        `;
        tbody.appendChild(tr);
    });

    table.appendChild(tbody);
    container.appendChild(table);

    if (bestDayNoteEl) {
        bestDayNoteEl.innerHTML = `<strong>Best draft day:</strong> ${best.dateStr} (${best.count} team${best.count === 1 ? '' : 's'} with a conflict &mdash; closest to the 27th among the lowest-conflict days)`;
    }
}

function escapeHtmlLocal(str) {
    if (!str) return '';
    return str
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}
