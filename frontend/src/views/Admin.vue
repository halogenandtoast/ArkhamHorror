<script setup lang="ts">
import { ref } from 'vue'
import api from '@/api'
import GameRow from '@/arkham/components/GameRow.vue'
import type { GameDetails } from '@/arkham/types/Game'

interface AdminData {
  currentUsers: number
  activeUsers: number
  activeGames: GameDetails[]
}

const request = await api.get<AdminData>('admin')
const data = request.data

const activeGames   = data.activeGames.filter(g => !g.error && g.gameState.tag !== 'IsOver')
const finishedGames = data.activeGames.filter(g => !g.error && g.gameState.tag === 'IsOver')

const sidebarOpen = ref(false)
const toggleSidebar = () => (sidebarOpen.value = !sidebarOpen.value)
</script>

<template>
  <div class="admin-shell" :class="{ 'sidebar-open': sidebarOpen }">
    <!-- Sidebar -->
    <aside class="sidebar">
      <div class="brand">
        <span>Admin</span>
      </div>

      <nav class="nav">
        <a class="nav-link active" href="#">
          <!-- dashboard icon -->
          <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M3 13h8V3H3v10zm0 8h8v-6H3v6zm10 0h8V11h-8v10zm0-18v6h8V3h-8z" fill="currentColor"/></svg>
          Dashboard
        </a>
        <a class="nav-link" href="#">
          <!-- users icon -->
          <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M16 11c1.66 0 2.99-1.34 2.99-3S17.66 5 16 5s-3 1.34-3 3 1.34 3 3 3zM8 11c1.66 0 2.99-1.34 2.99-3S9.66 5 8 5 5 6.34 5 8s1.34 3 3 3zm0 2c-2.33 0-7 1.17-7 3.5V20h14v-3.5C15 14.17 10.33 13 8 13zm8 0c-.29 0-.62.02-.97.05 1.16.84 1.97 1.98 1.97 3.45V20h6v-3.5c0-2.33-4.67-3.5-7-3.5z" fill="currentColor"/></svg>
          Users
        </a>
        <a class="nav-link" href="#">
          <!-- games icon -->
          <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M21 6h-7V3H3v15h3v3h15V6zm-2 13H8v-3H6V5h6v3h7v11z" fill="currentColor"/></svg>
          Games
        </a>

        <div class="nav-section">Insights</div>
        <a class="nav-link" href="#"><svg viewBox="0 0 24 24"><path d="M3 13h2v-2H3v2zm4 0h14v-2H7v2zM3 17h2v-2H3v2zm4 0h14v-2H7v2zM3 9h2V7H3v2zm4 0h14V7H7v2z" fill="currentColor"/></svg>Reports</a>
        <a class="nav-link" href="#"><svg viewBox="0 0 24 24"><path d="M11 17h2v-6h-2v6zm0-8h2V7h-2v2zm1-7C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2z" fill="currentColor"/></svg>Status</a>
      </nav>

      <div class="sidebar-footer">
        <button class="collapse" @click="toggleSidebar" aria-label="Toggle sidebar">
          <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M10 6l6 6-6 6V6z" fill="currentColor"/></svg>
        </button>
      </div>
    </aside>

    <!-- Main -->
    <main class="content">
      <header class="topbar">
        <button class="hamburger" @click="toggleSidebar" aria-label="Open menu">
          <svg viewBox="0 0 24 24"><path d="M3 6h18v2H3V6zm0 10h18v2H3v-2zm0-5h18v2H3v-2z" fill="currentColor"/></svg>
        </button>
        <h1>Dashboard</h1>
      </header>

      <!-- Cards row -->
      <section class="cards-row">
        <div class="card kpi accent-blue">
          <div class="kpi-head">Current Users</div>
          <div class="kpi-value">{{ data.currentUsers }}</div>
        </div>
        <div class="card kpi accent-purple">
          <div class="kpi-head">Active Users (14d)</div>
          <div class="kpi-value">{{ data.activeUsers }}</div>
        </div>
        <div class="card kpi accent-green">
          <div class="kpi-head">Active Games</div>
          <div class="kpi-value">{{ activeGames.length }}</div>
        </div>
        <div class="card kpi accent-orange">
          <div class="kpi-head">Finished Games</div>
          <div class="kpi-value">{{ finishedGames.length }}</div>
        </div>
      </section>

      <!-- Active Games -->
      <section class="block">
        <div class="block-header">
          <h2>Active Games</h2>
        </div>
        <div v-if="activeGames.length === 0" class="empty">No active games.</div>
        <div class="game-list">
          <GameRow v-for="g in activeGames" :key="g.id" :game="g" :admin="true" />
        </div>
      </section>

      <!-- Finished Games -->
      <section class="block" v-if="finishedGames.length > 0">
        <div class="block-header">
          <h2>Finished Games</h2>
        </div>
        <div class="game-list">
          <GameRow v-for="g in finishedGames" :key="g.id" :game="g" :admin="true" />
        </div>
      </section>
    </main>
  </div>
</template>

<style scoped>
/* ---------------- Theme ---------------- */
:host, .admin-shell {
  --bg:        #0b0d10;
  --panel:     #12151a;
  --panel-2:   #151a21;
  --text:      #e7eaef;
  --muted:     #9aa3b2;
  --line:      #252b34;
  --brand:     #7dd3fc;
  --brand-2:   #a78bfa;
  --shadow:    0 12px 32px rgba(0,0,0,.35);
}

/* ---------------- Layout ---------------- */
.admin-shell {
  display: grid;
  grid-template-columns: 260px 1fr;
  grid-template-rows: 100vh; /* sidebar stretches full height */
  background: var(--bg);
  color: var(--text);
  overflow: hidden;
}

/* Mobile/Tablet: sidebar slides over */
@media (max-width: 960px) {
  .admin-shell {
    grid-template-columns: 1fr;
  }
}

/* ---------------- Sidebar ---------------- */
.sidebar {
  position: sticky;
  top: 0;
  height: 100vh;
  display: flex; flex-direction: column;
  gap: 12px;
  padding: 18px 14px;
  background:
    radial-gradient(700px 300px at 0% 0%, rgba(125,211,252,.06), transparent 60%),
    linear-gradient(180deg, var(--panel), var(--panel-2));
  border-right: 1px solid var(--line);
  box-shadow: inset -1px 0 0 var(--line);
  z-index: 20;
}

.brand {
  display: flex; align-items: center; gap: 10px;
  font-weight: 800; letter-spacing: .02em;
  color: var(--text);
}

/* nav */
.nav {
  display: grid;
  gap: 4px;
  margin-top: 8px;
}
.nav-section {
  margin: 14px 10px 6px;
  font-size: .75rem;
  color: var(--muted);
  letter-spacing: .12em;
  text-transform: uppercase;
}
.nav-link {
  display: grid;
  grid-template-columns: 22px 1fr;
  align-items: center;
  gap: 12px;
  padding: 10px 12px;
  border-radius: 12px;
  color: var(--text);
  text-decoration: none;
  border: 1px solid transparent;
  transition: background .18s ease, border-color .18s ease, transform .18s ease;
}
.nav-link svg { width: 18px; height: 18px; opacity: .9 }
.nav-link:hover {
  background: color-mix(in oklab, var(--brand) 14%, transparent);
  border-color: color-mix(in oklab, var(--brand) 26%, transparent);
  transform: translateX(2px);
}
.nav-link.active {
  background: color-mix(in oklab, var(--brand) 20%, transparent);
  border-color: color-mix(in oklab, var(--brand) 40%, transparent);
  box-shadow: 0 8px 20px rgba(125,211,252,.15) inset;
}

/* sidebar footer */
.sidebar-footer {
  margin-top: auto;
  display: flex; justify-content: flex-end;
}
.collapse {
  display: grid; place-items: center;
  width: 36px; height: 36px;
  border-radius: 10px;
  background: rgba(255,255,255,.04);
  border: 1px solid var(--line);
  color: var(--text);
}
.collapse:hover { background: rgba(255,255,255,.07) }

/* Slide-in behavior on small screens */
@media (max-width: 960px) {
  .sidebar {
    position: fixed; left: 0; top: 0;
    transform: translateX(-100%);
    width: 260px;
    transition: transform .25s ease;
  }
  .admin-shell.sidebar-open .sidebar {
    transform: translateX(0);
  }
}

/* ---------------- Main content ---------------- */
.content {
  min-width: 0; /* fix overflow with grids */
  display: flex; flex-direction: column;
  height: 100vh; overflow: auto;
  background:
    radial-gradient(1200px 600px at 100% 0%, rgba(167,139,250,.05), transparent 60%),
    var(--bg);
}

/* top bar */
.topbar {
  position: sticky; top: 0; z-index: 10;
  display: flex; align-items: center; gap: 12px;
  padding: 14px 20px;
  border-bottom: 1px solid var(--line);
  background: color-mix(in oklab, var(--bg) 85%, transparent);
  backdrop-filter: blur(6px);
}
.topbar h1 { font-size: 1rem; margin: 0; color: var(--text); font-weight: 700; letter-spacing: .02em }
.hamburger {
  display: none;
  width: 36px; height: 36px; border-radius: 10px;
  background: rgba(255,255,255,.04);
  border: 1px solid var(--line);
  color: var(--text);
}
.hamburger:hover { background: rgba(255,255,255,.07) }

@media (max-width: 960px) {
  .hamburger { display: grid; place-items: center }
}

/* KPI cards row */
.cards-row {
  display: grid;
  grid-template-columns: repeat(4, minmax(180px, 1fr));
  gap: 16px;
  padding: 20px;
}
@media (max-width: 1200px) {
  .cards-row { grid-template-columns: repeat(2, minmax(180px, 1fr)); }
}
@media (max-width: 600px) {
  .cards-row { grid-template-columns: 1fr; }
}

.card.kpi {
  background: linear-gradient(180deg, var(--panel), var(--panel-2));
  border: 1px solid var(--line);
  border-radius: 14px;
  padding: 16px;
  box-shadow: var(--shadow);
  transition: transform .18s ease, box-shadow .18s ease, border-color .18s ease;

  &.accent-blue {
    background: linear-gradient(180deg, var(--panel), rgba(125,211,252,.15));
  }

  &.accent-purple {
    background: linear-gradient(180deg, var(--panel), rgba(167,139,250,.15));
  }

  &.accent-green {
    background: linear-gradient(180deg, var(--panel), rgba(52,211,153,.15));
  }

  &.accent-orange {
    background: linear-gradient(180deg, var(--panel), rgba(249,115,22,.15));
  }
}

.card.kpi:hover {
  transform: translateY(-2px);
  border-color: color-mix(in oklab, var(--brand) 30%, transparent);
  box-shadow: 0 14px 36px rgba(0,0,0,.45);
}
.kpi-head { color: var(--muted); font-size: .85rem; margin-bottom: 6px; letter-spacing: .02em }
.kpi-value { font-size: 2rem; font-weight: 800; }

/* Blocks & lists */
.block { padding: 8px 20px 24px }
.block-header {
  position: sticky; top: 54px; /* below topbar */
  display: flex; align-items: center; justify-content: space-between;
  padding: 8px 0 10px;
  border-bottom: 1px solid var(--line);
  background: color-mix(in oklab, var(--bg) 85%, transparent);
  backdrop-filter: blur(6px);
  z-index: 5;
}
.block-header h2 {
  font-size: .95rem; text-transform: uppercase; letter-spacing: .12em;
  color: var(--muted); margin: 0;
}

.game-list { display: grid; gap: 10px; margin-top: 12px }
.game-list :deep(> *) {
  border-radius: 12px;
  border: 1px solid var(--line);
  background: linear-gradient(180deg, var(--panel), var(--panel-2));
  box-shadow: var(--shadow);
}

/* Empty state */
.empty {
  display: grid; place-items: center;
  padding: 24px; margin-top: 12px;
  border-radius: 12px;
  color: var(--muted);
  border: 1px dashed rgba(168,176,191,.3);
  background:
    repeating-linear-gradient(135deg, rgba(125,211,252,.04) 0 10px, transparent 10px 20px),
    var(--panel);
}
</style>
