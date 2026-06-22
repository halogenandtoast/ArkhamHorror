<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import api from '@/api'
import GameRow from '@/arkham/components/GameRow.vue'
import GameFinder from '@/components/admin/GameFinder.vue'
import type { GameDetails, GameDetailsEntry } from '@/arkham/types/Game'

type AdminGameDetailsEntry = GameDetails | GameDetailsEntry | { error: string }

interface AdminStats {
  currentUsers: number
  activeUsers: number
}

const stats = ref<AdminStats | null>(null)
const activeGameEntries = ref<AdminGameDetailsEntry[]>([])
const recentGameEntries = ref<AdminGameDetailsEntry[]>([])

const statsLoading = ref(true)
const activeGamesLoading = ref(true)
const recentGamesLoading = ref(true)

const statsError = ref(false)
const activeGamesError = ref(false)
const recentGamesError = ref(false)

const isGameDetails = (entry: AdminGameDetailsEntry): entry is GameDetails => !('error' in entry)

const activeGameDetails = computed(() => activeGameEntries.value.filter(isGameDetails))
const recentGameDetails = computed(() => recentGameEntries.value.filter(isGameDetails))

const activeGames = computed(() => activeGameDetails.value.filter(g => g.gameState.tag !== 'IsOver'))
const finishedGames = computed(() => activeGameDetails.value.filter(g => g.gameState.tag === 'IsOver'))

const recentActiveGames = computed(() => recentGameDetails.value.filter(g => g.gameState.tag !== 'IsOver'))
const recentFinishedGames = computed(() => recentGameDetails.value.filter(g => g.gameState.tag === 'IsOver'))

async function loadStats() {
  try {
    stats.value = (await api.get<AdminStats>('admin/stats')).data
  } catch {
    statsError.value = true
  } finally {
    statsLoading.value = false
  }
}

async function loadActiveGames() {
  try {
    activeGameEntries.value = (await api.get<AdminGameDetailsEntry[]>('admin/games/active')).data
  } catch {
    activeGamesError.value = true
  } finally {
    activeGamesLoading.value = false
  }
}

async function loadRecentGames() {
  try {
    recentGameEntries.value = (await api.get<AdminGameDetailsEntry[]>('admin/games')).data
  } catch {
    recentGamesError.value = true
  } finally {
    recentGamesLoading.value = false
  }
}

onMounted(() => {
  void loadStats()
  void loadActiveGames()
  void loadRecentGames()
})
</script>

<template>
  <section class="stats-grid" aria-label="Admin stats">
      <div class="stat-card box">
        <span class="stat-label">Current Users</span>
        <strong>{{ statsLoading ? '…' : stats?.currentUsers ?? '—' }}</strong>
      </div>
      <div class="stat-card box">
        <span class="stat-label">Active Users (14d)</span>
        <strong>{{ statsLoading ? '…' : stats?.activeUsers ?? '—' }}</strong>
      </div>
      <div class="stat-card box">
        <span class="stat-label">Active Games</span>
        <strong>{{ activeGamesLoading ? '…' : activeGames.length }}</strong>
      </div>
      <div class="stat-card box">
        <span class="stat-label">Finished Games</span>
        <strong>{{ activeGamesLoading ? '…' : finishedGames.length }}</strong>
      </div>
    </section>

    <p v-if="statsError" class="empty box">Unable to load admin stats.</p>

    <section class="admin-block">
      <header class="section-header">
        <h2>Find Game</h2>
      </header>
      <GameFinder />
    </section>

    <section class="admin-block">
      <header class="section-header">
        <h2>Active Games</h2>
        <span class="count-badge" aria-label="Active games count">{{ activeGames.length }}</span>
      </header>
      <div v-if="activeGamesLoading" class="empty box">Loading active games…</div>
      <div v-else-if="activeGamesError" class="empty box">Unable to load active games.</div>
      <div v-else-if="activeGames.length === 0" class="empty box">No active games.</div>
      <div v-else class="game-list">
        <GameRow v-for="g in activeGames" :key="g.id" :game="g" :admin="true" />
      </div>
    </section>

    <section class="admin-block" v-if="finishedGames.length > 0">
      <header class="section-header">
        <h2>Finished Games</h2>
        <span class="count-badge" aria-label="Finished games count">{{ finishedGames.length }}</span>
      </header>
      <div class="game-list">
        <GameRow v-for="g in finishedGames" :key="g.id" :game="g" :admin="true" />
      </div>
    </section>

    <section class="admin-block">
      <header class="section-header">
        <h2>Recent Active Games</h2>
        <span class="section-note">from last 20</span>
        <span class="count-badge" aria-label="Recent active games count">{{ recentActiveGames.length }}</span>
      </header>
      <div v-if="recentGamesLoading" class="empty box">Loading recent games…</div>
      <div v-else-if="recentGamesError" class="empty box">Unable to load recent games.</div>
      <div v-else-if="recentActiveGames.length === 0" class="empty box">No recent active games.</div>
      <div v-else class="game-list">
        <GameRow v-for="g in recentActiveGames" :key="g.id" :game="g" :admin="true" />
      </div>
    </section>

    <section class="admin-block" v-if="recentFinishedGames.length > 0">
      <header class="section-header">
        <h2>Recent Finished Games</h2>
        <span class="section-note">from last 20</span>
        <span class="count-badge" aria-label="Recent finished games count">{{ recentFinishedGames.length }}</span>
      </header>
      <div class="game-list">
        <GameRow v-for="g in recentFinishedGames" :key="g.id" :game="g" :admin="true" />
      </div>
    </section>
</template>

<style scoped>
.stats-grid {
  display: grid;
  gap: 10px;
  grid-template-columns: repeat(4, minmax(0, 1fr));
}

.stat-card {
  display: flex;
  flex-direction: column;
  gap: 6px;
  min-height: 82px;
  justify-content: center;
}

.stat-label {
  color: color-mix(in srgb, var(--title) 70%, transparent);
  font-size: 0.8rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.stat-card strong {
  color: var(--title);
  font-family: "Noto Sans", Avenir, Helvetica, Arial, sans-serif;
  font-size: 2.1rem;
  font-weight: 800;
  line-height: 1;
}

.admin-block {
  background: color-mix(in srgb, var(--background-dark) 42%, transparent);
  border: 1px solid color-mix(in srgb, var(--box-border) 75%, transparent);
  border-radius: 6px;
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 14px;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
}

.admin-block + .admin-block {
  margin-top: 4px;
}

.section-header {
  align-items: center;
  display: flex;
  gap: 12px;
}

.section-header h2 {
  color: var(--title);
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 1.6rem;
  line-height: 1;
  margin: 0;
  text-transform: uppercase;
}

.section-note {
  color: color-mix(in srgb, var(--title) 60%, transparent);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.count-badge {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-left-width: 4px;
  border-radius: 3px;
  color: color-mix(in srgb, var(--spooky-green) 78%, white);
  display: inline-flex;
  font-size: 0.78rem;
  font-weight: 800;
  justify-content: center;
  line-height: 1;
  min-width: 2.1em;
  padding: 5px 9px 5px 7px;
}

.game-list,
.room-list {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.empty {
  color: var(--title);
  opacity: 0.75;
}

@media (max-width: 900px) {
  .stats-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

@media (max-width: 520px) {
  .stats-grid {
    grid-template-columns: 1fr;
  }
}
</style>
