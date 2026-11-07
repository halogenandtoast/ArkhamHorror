<script lang="ts" setup>
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import type { Location } from '@/arkham/types/Location'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { useStorage } from '@vueuse/core'
import { cardArt } from '@/arkham/cardImages'
import { hemlockHousePosition, hemlockOverview, hemlockVillagePosition } from '@/arkham/hemlock-overview'
import { useSettings, type LocationView } from '@/stores/settings'
import {
  gridPositions, locationLinks, mapPositions, mineDescentPositions, shortestPath,
  type LocationGraph, type Point,
} from '@/arkham/locationGraph'
import { gridSlotIndex } from '@/arkham/locationChoices'
import LocationMapCanvas, { type MapMove, type RoomPlateSpot } from '@/arkham/components/LocationMapCanvas.vue'
import LocationActionPanel from '@/arkham/components/LocationActionPanel.vue'
import LocationHand from '@/arkham/components/LocationHand.vue'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import LocationMissionStrip from '@/arkham/components/LocationMissionStrip.vue'
import LocationInfoCard from '@/arkham/components/LocationInfoCard.vue'
import LocationMapLegend from '@/arkham/components/LocationMapLegend.vue'
import { hasMapOffsets, resetMapOffsets } from '@/arkham/composables/useMapDrag'
import { useLocationInfo } from '@/arkham/composables/useLocationInfo'

const props = defineProps<{
  game: Game
  scenario: Scenario
  playerId: string
  view: Exclude<LocationView, 'cards'>
  /** The scenario shows things only the card board can draw (barriers, enemies standing as locations, ...). */
  boardOnlyContent: boolean
  decksShown: boolean
  /** The scenario reference card, shown next to the act and agenda. */
  referenceImage?: string
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

// Enemies engaged with an investigator are in that investigator's threat area, not on the location: they still stand in the room.
const enemiesShownAt = (l: Location) => [...new Set([
  ...enemiesAt(l),
  ...l.investigators.flatMap((id) => props.game.investigators[id]?.engagedEnemies ?? []),
])].filter((id) => props.game.enemies[id])

const graph = computed<LocationGraph>(() => Object.fromEntries(locations.value.map((l) => [l.id, {
  id: l.id, label: l.label, connectedLocations: l.connectedLocations,
  investigators: l.investigators, enemies: enemiesAt(l),
}])))

const isStudyOnly = computed(() =>
  locations.value.length === 1 && cardArt(locations.value[0].cardCode) === '01111'
)

const illustratedBoard = computed(() => {
  if (isStudyOnly.value) {
    return { image: 'img/scenes/night-of-the-zealot/study.webp', fixedPassages: false, village: true }
  }
  return hemlockOverview(
    props.scenario.id, props.scenario.reference, props.scenario.locationLayout,
  )
})

const layout = computed(() => props.scenario.usesGrid || props.scenario.locationLayout ? props.scenario.locationLayout : null)
const positions = computed<Record<string, Point>>(() => {
  if (isStudyOnly.value) {
    return { [locations.value[0].id]: [50, 75] }
  }
  const fallback = mapPositions(graph.value, layout.value)
  if (illustratedBoard.value?.fixedPassages) return mineDescentPositions(graph.value, fallback)
  const board = illustratedBoard.value
  if (!board?.village && !board?.house) return fallback
  return Object.fromEntries(locations.value.map((location) => [
    location.id,
    (board.house ? hemlockHousePosition(location.label) : hemlockVillagePosition(location.cardCode)) ?? fallback[location.id],
  ]))
})
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
    .map(([label, [x, y]]) => {
      const anchor = illustratedBoard.value?.house ? hemlockHousePosition(label) : undefined
      return { label, x: anchor?.[0] ?? x, y: anchor?.[1] ?? (squeeze ? y * 0.8 : y), index: gridSlotIndex(choices.value, label) }
    })
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

// The Gathering's house, once more than the study is in play: draw the rooms on the cutaway picture of the house.
// Each room has a spot for its people (bottom centre, on the floor) and a corner for its name plate
// (top left, percent of the picture, with a width), so the plate never sits under the figures.
const HOUSE_ROOMS: Record<string, { figures: Point; plate: RoomPlateSpot }> = {
  '01111': { figures: [52, 81], plate: { x: 64, y: 66, width: 16 } },
  '01112': { figures: [62, 61], plate: { x: 36, y: 31, width: 17 } },
  '01113': { figures: [47, 25], plate: { x: 62, y: 4, width: 16 } },
  '01114': { figures: [55, 96], plate: { x: 33, y: 67, width: 15 } },
  '01115': { figures: [84, 63], plate: { x: 74.5, y: 33, width: 18 } },
}
const houseRooms = computed(() => {
  const all = locations.value
  if (all.length < 2 || !all.every((l) => cardArt(l.cardCode) in HOUSE_ROOMS)) return null
  return all.map((l) => [l.id, HOUSE_ROOMS[cardArt(l.cardCode)]] as const)
})
const cutaway = computed<Record<string, Point> | null>(() =>
  houseRooms.value ? Object.fromEntries(houseRooms.value.map(([id, room]) => [id, room.figures])) : null)
const roomPlates = computed<Record<string, RoomPlateSpot> | null>(() =>
  houseRooms.value ? Object.fromEntries(houseRooms.value.map(([id, room]) => [id, room.plate])) : null)
const overview = useStorage('arkham-scene-overview', true)
// Card text and flavour on the room plates: all shown or all folded away, so a small room keeps space for its figures.
const plateText = useStorage('arkham-room-plate-text', false)
const showOverview = computed(() =>
  props.view === 'map' || !!illustratedBoard.value || (cutaway.value !== null && overview.value)
)

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

const buildInfo = useLocationInfo(() => props.game, () => props.playerId, () => choices.value, enemiesShownAt)
// Built once per update, so every plate, the scene and the card read the same snapshot.
const infos = computed(() => new Map(locations.value.map((l) => [l.id, buildInfo(l)])))
const infoOf = (l: Location) => infos.value.get(l.id) ?? buildInfo(l)

// --- the info card: looking at a location never sends a choice, picking one is a separate click ---
const inspectedId = ref<string | null>(null)
const inspected = computed(() => {
  const id = inspectedId.value
  return id && props.game.locations[id] && graph.value[id] ? props.game.locations[id] : null
})
const inspectedConnections = computed(() => (inspected.value?.connectedLocations ?? [])
  .map((id) => props.game.locations[id]).filter((l): l is Location => Boolean(l) && Boolean(graph.value[l.id]))
  .map((l) => { const info = infoOf(l); return { id: l.id, name: info.face.name, revealed: info.revealed } }))
const panelOpen = ref(true)
// The card sits in the top-right corner, left of the actions panel; focus goes back to the plate that opened it.
const anchorEl = ref<HTMLElement | null>(null)

function inspect(l: Location, anchor: HTMLElement | null) {
  selectedId.value = l.id
  inspectedId.value = l.id
  if (anchor) anchorEl.value = anchor
}
function inspectById(id: string, anchor: HTMLElement | null = null) {
  const l = props.game.locations[id]
  if (l) inspect(l, anchor)
}
function closeCard() {
  const back = anchorEl.value?.querySelector<HTMLElement>('button') ?? anchorEl.value
  inspectedId.value = null
  back?.focus()
}
// The card is rebuilt from the live state; once its location leaves the map it closes.
watch(inspected, (l) => { if (!l) inspectedId.value = null })

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

const views: LocationView[] = ['map', 'cards']
</script>

<template>
  <section class="location-map" :aria-label="t('locationMap.title')">
    <div class="map-bar">
      <span class="views" role="tablist">
        <button v-for="v in views" :key="v" class="view" :class="{ active: v === view }" role="tab"
          :aria-selected="v === view" @click="settings.setLocationView(v)">{{ t(`locationMap.view.${v}`) }}</button>
      </span>
      <span v-if="boardOnlyContent" class="board-note">{{ t('locationMap.boardOnly') }}</span>
      <button class="view" :class="{ active: panelOpen }" @click="panelOpen = !panelOpen">{{ t('locationMap.actions') }}</button>
      <button v-if="showOverview && roomPlates" class="view" :class="{ active: plateText }" :aria-pressed="plateText"
        @click="plateText = !plateText">{{ plateText ? t('locationMap.marker.collapseAll') : t('locationMap.marker.expandAll') }}</button>
      <button v-if="hasMapOffsets(game.id)" class="view" @click="resetMapOffsets(game.id)">{{ t('locationMap.resetLayout') }}</button>
      <LocationMapLegend />
      <button class="view decks" :class="{ active: decksShown }" @click="emit('toggleDecks')">{{ t('locationMap.decks') }}</button>
    </div>
    <LocationMissionStrip :game="game" :scenario="scenario" :playerId="playerId" :referenceImage="referenceImage" @choose="emit('choose', $event)" />
    <div class="map-layout" :class="[view, { 'panel-open': panelOpen }]" @click="inspectedId = null">
      <div id="scene-team-slot" class="team-slot"></div>
      <LocationMapCanvas :cutaway="cutaway" :roomPlates="roomPlates" :plateText="plateText" :illustrated-board="illustratedBoard" :game="game" :playerId="playerId" :locations="locations"
        :positions="positions" :links="links" :moves="moves" :gridSpots="gridSpots" :selectedId="selected?.id ?? null"
        :enemiesAt="enemiesShownAt" :infoOf="infoOf" :width="canvasSize.width" :height="canvasSize.height"
        @inspect="inspect" @choose="emit('choose', $event)" />
      <LocationActionPanel v-if="panelOpen" class="action-overlay" :game="game" :playerId="playerId"
        :location="game.locations[myLocation ?? ''] ?? selected" @choose="emit('choose', $event)" @close="panelOpen = false" />
      <LocationInfoCard v-if="inspected" :game="game" :playerId="playerId" :location="inspected" :info="infoOf(inspected)"
        :connections="inspectedConnections" :rightGap="panelOpen ? 300 : 0" @choose="emit('choose', $event)"
        @inspect="inspectById" @close="closeCard" />
    </div>
    <LocationHand :game="game" :playerId="playerId" @choose="emit('choose', $event)" />
  </section>
</template>

<style scoped>
.location-map { display: flex; flex-direction: column; min-height: 380px; height: calc(100dvh - 60px - var(--hand-h, 0px)); flex: none; overflow: hidden; background: radial-gradient(ellipse at 50% 45%, #2c3a28, #0e1610 78%); }
.map-bar { display: flex; justify-content: space-between; align-items: center; gap: 12px; padding: 5px 12px; background: #101a14; border-bottom: 1px solid #a68b5540; }
.views { display: flex; gap: 4px; }
.view { padding: 3px 12px; font-size: 0.75rem; letter-spacing: 0.08em; text-transform: uppercase; background: transparent; border: 1px solid #75836b66; border-radius: 4px; color: #b4bdab; cursor: pointer; }
.view:hover { border-color: #e6c584; }
.view.active { background: #c6a873; border-color: #c6a873; color: #191c16; font-weight: 650; }
.board-note { font-size: 0.75rem; color: #d8b68c; }
.map-layout { position: relative; display: flex; flex: 1 1 300px; min-height: 300px; }
.map-layout > .map-scroll { box-sizing: border-box; }
.map-layout.panel-open > .map-scroll { padding-right: 300px; }
.team-slot:not(:empty) ~ .map-scroll { padding-left: 212px; }
.action-overlay { position: absolute; right: 12px; top: 12px; bottom: 12px; height: auto; z-index: 60; }
.mini-map { position: absolute; left: 12px; bottom: 12px; z-index: 50; width: clamp(240px, 24vw, 380px); aspect-ratio: 4 / 3; padding: 6px; background: #0b130ee6; border: 1px solid #c6a873aa; border-radius: 8px; box-shadow: 0 6px 20px #000a; }
.mini-map :deep(.map-scroll) { width: 100%; height: 100%; }
.map-layout > :not(.team-slot):not(.action-overlay):not(.mini-map) { flex: 1; min-width: 0; }
.team-slot { position: absolute; left: 12px; top: 84px; bottom: 12px; z-index: 55; width: 242px; display: flex; flex-direction: column; pointer-events: none; }
@media (max-width: 800px) { .team-slot { width: calc(100% - 24px); bottom: auto; } }
@media (max-width: 800px) {
  .location-map { height: auto; overflow: visible; }
  .map-layout { height: auto; }
  .map-layout > .scene { height: 60vh; }
}
</style>
