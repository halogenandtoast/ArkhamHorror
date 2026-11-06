<script lang="ts" setup>
import { computed, ref, watch, onBeforeUnmount } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import type { Location } from '@/arkham/types/Location'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { useSettings, type LocationView } from '@/stores/settings'
import {
  gridPositions, locationLinks, mapPositions, shortestPath,
  type LocationGraph, type Point,
} from '@/arkham/locationGraph'
import { gridSlotIndex, selectLocationIndex } from '@/arkham/locationChoices'
import LocationMapCanvas, { type MapMove } from '@/arkham/components/LocationMapCanvas.vue'
import LocationActionList from '@/arkham/components/LocationActionList.vue'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import LocationMissionStrip from '@/arkham/components/LocationMissionStrip.vue'
import LocationScene from '@/arkham/components/LocationScene.vue'

const props = defineProps<{
  game: Game
  scenario: Scenario
  playerId: string
  view: Exclude<LocationView, 'cards'>
  /** The scenario shows things only the card board can draw (barriers, enemies standing as locations, ...). */
  boardOnlyContent: boolean
  decksShown: boolean
}>()
const emit = defineEmits<{ (e: 'choose', index: number): void; (e: 'toggleDecks'): void }>()

const { t } = useI18n()
const nameOf = useCardNames()
const settings = useSettings()
const choices = useGameChoices(() => props.game, () => props.playerId)

// The same set the card board draws: attached locations still sit on the map, only ones taken off it are left out.
const locations = computed(() => Object.values(props.game.locations)
  .filter((l) => (l.placement === null || l.placement.tag === 'AttachedToLocation') && l.label !== 'cosmos'))

const enemiesAt = (l: Location) => l.enemies.filter((id) => props.game.enemies[id]?.placement.tag === 'AtLocation')

const graph = computed<LocationGraph>(() => Object.fromEntries(locations.value.map((l) => [l.id, {
  id: l.id, label: l.label, connectedLocations: l.connectedLocations,
  investigators: l.investigators, enemies: enemiesAt(l),
}])))

const layout = computed(() => props.scenario.usesGrid || props.scenario.locationLayout ? props.scenario.locationLayout : null)
const positions = computed<Record<string, Point>>(() => mapPositions(graph.value, layout.value))
const links = computed(() => locationLinks(graph.value))

// Empty grid slots a choice can fill (placing a location), drawn as dashed outlines.
const gridSpots = computed(() => {
  const rows = layout.value
  if (!rows || !props.scenario.usesGrid) return []
  const used = new Set(locations.value.map((l) => l.label))
  const spots = gridPositions(rows)
  const squeeze = locations.value.some((l) => !(l.label in spots)) && locations.value.some((l) => l.label in spots)
  return Object.entries(spots)
    .filter(([label]) => !used.has(label) && gridSlotIndex(choices.value, label) !== -1)
    .map(([label, [x, y]]) => ({ label, x, y: squeeze ? y * 0.8 : y, index: gridSlotIndex(choices.value, label) }))
})

const canvasSize = computed(() => {
  const rows = layout.value?.map((r) => r.trim().split(/\s+/)) ?? []
  const n = locations.value.length
  const cols = rows.length ? Math.max(...rows.map((r) => r.length)) : Math.ceil(Math.sqrt(n * 1.6))
  const grid = gridPositions(layout.value ?? [])
  const stripRow = rows.length && locations.value.some((l) => !(l.label in grid)) ? 1 : 0
  const lines = (rows.length || Math.ceil(n / Math.max(cols, 1))) + stripRow
  return { width: Math.max(cols, 1) * 150, height: Math.max(lines, 2) * 120 }
})

const me = computed(() => ownInvestigator(props.game, props.playerId))
const myLocation = computed(() => me.value?.location ?? null)
const selectedId = ref<string | null>(null)
const selected = computed(() => {
  const id = selectedId.value
  if (id && props.game.locations[id] && graph.value[id]) return props.game.locations[id]
  const mine = myLocation.value
  if (mine && graph.value[mine]) return props.game.locations[mine]
  return locations.value[0] ?? null
})
// Following the investigator: once they move, the scene follows rather than staying on the room they left.
watch(myLocation, () => { selectedId.value = null })

const neighbours = computed(() => (selected.value?.connectedLocations ?? [])
  .map((id) => props.game.locations[id]).filter((l): l is Location => Boolean(l) && Boolean(graph.value[l.id])))

function activate(l: Location) {
  selectedId.value = l.id
  // A location being offered as a target is chosen on the spot, as on the card board.
  const index = selectLocationIndex(choices.value, l)
  if (index !== -1) emit('choose', index)
}

// --- enemy movement: a dot travels along the links an enemy took since the last update ---
const moves = ref<MapMove[]>([])
let previousEnemyAt: Record<string, string> | null = null
let previousScenario = props.scenario.id
let clearTimer: ReturnType<typeof setTimeout> | null = null

const reducedMotion = typeof window !== 'undefined' && window.matchMedia?.('(prefers-reduced-motion: reduce)').matches

watch(graph, (g) => {
  const now: Record<string, string> = {}
  for (const l of Object.values(g)) for (const e of l.enemies) now[e] = l.id
  if (props.scenario.id !== previousScenario) previousEnemyAt = null
  previousScenario = props.scenario.id
  const before = previousEnemyAt
  previousEnemyAt = now
  if (!before || reducedMotion) { moves.value = []; return }
  moves.value = Object.entries(now).flatMap(([enemyId, to]) => {
    const from = before[enemyId]
    if (!from || from === to) return []
    const path = shortestPath(g, from, to)
    const points = path.map((id) => positions.value[id]).filter(Boolean)
    if (points.length < 2) return []
    const enemy = props.game.enemies[enemyId]
    return [{ id: enemyId, name: enemy ? nameOf(enemy.cardCode) : '', path: points.map(([x, y], i) => `${i ? 'L' : 'M'}${x},${y}`).join(' ') }]
  })
  if (clearTimer) clearTimeout(clearTimer)
  clearTimer = setTimeout(() => { moves.value = [] }, 1800)
}, { immediate: true })

onBeforeUnmount(() => { if (clearTimer) clearTimeout(clearTimer) })

const views: LocationView[] = ['scene', 'map', 'cards']
</script>

<template>
  <section class="location-map" :aria-label="t('locationMap.title')">
    <div class="map-bar">
      <span class="views" role="tablist">
        <button v-for="v in views" :key="v" class="view" :class="{ active: v === view }" role="tab"
          :aria-selected="v === view" @click="settings.setLocationView(v)">{{ t(`locationMap.view.${v}`) }}</button>
      </span>
      <span v-if="boardOnlyContent" class="board-note">{{ t('locationMap.boardOnly') }}</span>
      <button class="view decks" :class="{ active: decksShown }" @click="emit('toggleDecks')">{{ t('locationMap.decks') }}</button>
    </div>
    <LocationMissionStrip :game="game" :scenario="scenario" :playerId="playerId" @choose="emit('choose', $event)" />
    <div class="map-layout" :class="view">
      <LocationScene v-if="view === 'scene' && selected" :game="game" :playerId="playerId" :location="selected"
        :neighbours="neighbours" :mine="selected.id === myLocation"
        @choose="emit('choose', $event)" @select="selectedId = $event" />
      <LocationMapCanvas v-else-if="view === 'map'" :game="game" :playerId="playerId" :locations="locations"
        :positions="positions" :links="links" :moves="moves" :gridSpots="gridSpots" :selectedId="selected?.id ?? null"
        :myLocation="myLocation" :choices="choices" :enemiesAt="enemiesAt" :width="canvasSize.width" :height="canvasSize.height"
        @activate="activate" @choose="emit('choose', $event)" />
    </div>
    <LocationActionList :game="game" :playerId="playerId" @choose="emit('choose', $event)" />
  </section>
</template>

<style scoped>
.location-map { display: flex; flex-direction: column; min-height: 0; flex: 1; background: radial-gradient(ellipse at 50% 45%, #2c3a28, #0e1610 78%); }
.map-bar { display: flex; justify-content: space-between; align-items: center; gap: 12px; padding: 5px 12px; background: #101a14; border-bottom: 1px solid #a68b5540; }
.views { display: flex; gap: 4px; }
.view { padding: 3px 12px; font-size: 0.75rem; letter-spacing: 0.08em; text-transform: uppercase; background: transparent; border: 1px solid #75836b66; border-radius: 4px; color: #b4bdab; cursor: pointer; }
.view:hover { border-color: #e6c584; }
.view.active { background: #c6a873; border-color: #c6a873; color: #191c16; font-weight: 650; }
.board-note { font-size: 0.75rem; color: #d8b68c; }
.map-layout { display: flex; min-height: 0; flex: 1; height: clamp(420px, 66vh, 780px); }
.map-layout > :first-child { min-height: 0; flex: 1; min-width: 0; }
@media (max-width: 800px) {
  .map-layout { height: auto; }
  .map-layout > :first-child { height: 60vh; }
}
</style>
