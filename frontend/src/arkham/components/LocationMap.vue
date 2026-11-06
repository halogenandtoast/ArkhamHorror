<script lang="ts" setup>
import { computed, ref, watch, onBeforeUnmount } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Scenario } from '@/arkham/types/Scenario'
import type { Location } from '@/arkham/types/Location'
import { TokenType } from '@/arkham/types/Token'
import { cardArt, cardImage } from '@/arkham/cardImages'
import { useGameChoices } from '@/arkham/composables/useGameChoices'
import { useDbCardStore } from '@/stores/dbCards'
import { useSettings } from '@/stores/settings'
import {
  gridPositions, locationLinks, mapPositions, shortestPath,
  type LocationGraph, type Point,
} from '@/arkham/locationGraph'
import { gridSlotIndex, selectLocationIndex } from '@/arkham/locationChoices'
import LocationMapPanel from '@/arkham/components/LocationMapPanel.vue'

const props = defineProps<{
  game: Game
  scenario: Scenario
  playerId: string
  /** The scenario shows things only the card board can draw (barriers, enemies standing as locations, ...). */
  boardOnlyContent: boolean
}>()
const emit = defineEmits<{ (e: 'choose', index: number): void }>()

const { t } = useI18n()
const dbCards = useDbCardStore()
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
  const lines = (rows.length || Math.ceil(n / Math.max(cols, 1))) + (locations.value.some((l) => !(l.label in gridPositions(layout.value ?? []))) && rows.length ? 1 : 0)
  return { width: Math.max(cols, 1) * 150, height: Math.max(lines, 2) * 120 }
})

const nameOf = (code: string) => dbCards.getDbCard(cardArt(code))?.name ?? code

const myLocation = computed(() => props.game.investigators[props.playerId]?.location ?? null)
const selectedId = ref<string | null>(null)
const selected = computed(() => {
  const id = selectedId.value
  if (id && props.game.locations[id] && graph.value[id]) return props.game.locations[id]
  const mine = myLocation.value
  if (mine && graph.value[mine]) return props.game.locations[mine]
  return locations.value[0] ?? null
})
const route = computed(() => selected.value && myLocation.value
  ? shortestPath(graph.value, myLocation.value, selected.value.id) : [])

function activate(l: Location) {
  selectedId.value = l.id
  // A location being offered as a target is chosen on the spot, as on the card board.
  const index = selectLocationIndex(choices.value, l)
  if (index !== -1) emit('choose', index)
}

const clueCount = (l: Location) => l.tokens[TokenType.Clue] ?? 0
const doomCount = (l: Location) => l.tokens[TokenType.Doom] ?? 0

// --- enemy movement: a dot travels along the links an enemy took since the last update ---
type Move = { id: string; name: string; path: string }
const moves = ref<Move[]>([])
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
    const route = shortestPath(g, from, to)
    const points = route.map((id) => positions.value[id]).filter(Boolean)
    if (points.length < 2) return []
    const enemy = props.game.enemies[enemyId]
    return [{ id: enemyId, name: enemy ? nameOf(enemy.cardCode) : '', path: points.map(([x, y], i) => `${i ? 'L' : 'M'}${x},${y}`).join(' ') }]
  })
  if (clearTimer) clearTimeout(clearTimer)
  clearTimer = setTimeout(() => { moves.value = [] }, 1800)
}, { immediate: true })

onBeforeUnmount(() => { if (clearTimer) clearTimeout(clearTimer) })
</script>

<template>
  <section class="location-map" :aria-label="t('locationMap.title')">
    <div class="map-bar">
      <span>{{ t('locationMap.title') }}</span>
      <button class="button" @click="settings.setLocationView('cards')">{{ t('locationMap.showCards') }}</button>
    </div>
    <p v-if="boardOnlyContent" class="board-note">{{ t('locationMap.boardOnly') }}</p>
    <div class="map-layout">
      <div class="map-scroll">
        <div class="map-canvas" :style="{ minWidth: canvasSize.width + 'px', height: canvasSize.height + 'px' }">
          <svg class="map-links" viewBox="0 0 100 100" preserveAspectRatio="none" aria-hidden="true">
            <line v-for="[a, b] in links" :key="a + b"
              :x1="positions[a][0]" :y1="positions[a][1]" :x2="positions[b][0]" :y2="positions[b][1]" />
            <circle v-for="m in moves" :key="m.id" class="moving-enemy" r="1.4">
              <title>{{ m.name }}</title>
              <animateMotion dur="1.4s" :path="m.path" fill="freeze" />
            </circle>
          </svg>
          <button v-for="spot in gridSpots" :key="spot.label" class="spot"
            :style="{ left: spot.x + '%', top: spot.y + '%' }"
            :aria-label="t('locationMap.placeHere')" @click="emit('choose', spot.index)">+</button>
          <button v-for="l in locations" :key="l.id" class="node"
            :class="{
              here: l.id === myLocation, selected: l.id === selected?.id,
              hidden: !l.revealed, targetable: selectLocationIndex(choices, l) !== -1,
            }"
            :style="{ left: positions[l.id][0] + '%', top: positions[l.id][1] + '%' }"
            :data-id="l.id" @click="activate(l)">
            <img class="art" :src="cardImage(l.cardCode, l.revealed ? '' : 'b')" alt="" loading="lazy" />
            <span class="title">{{ nameOf(l.cardCode) }}</span>
            <span class="badges">
              <span v-if="clueCount(l)" class="clue" :title="t('locationMap.clues')">◇{{ clueCount(l) }}</span>
              <span v-if="doomCount(l)" class="doom" :title="t('locationMap.doom')">☠{{ doomCount(l) }}</span>
            </span>
            <span class="occupants">
              <span v-for="id in l.investigators" :key="id" class="token investigator" :class="{ you: id === playerId }"
                :title="props.game.investigators[id] ? nameOf(props.game.investigators[id].cardCode) : id">
                {{ (props.game.investigators[id] ? nameOf(props.game.investigators[id].cardCode) : '?').charAt(0) }}
              </span>
              <span v-if="enemiesAt(l).length" class="token enemy">☠{{ enemiesAt(l).length > 1 ? ' ' + enemiesAt(l).length : '' }}</span>
            </span>
          </button>
        </div>
      </div>
      <LocationMapPanel :game="game" :playerId="playerId" :location="selected" :route="route"
        @choose="emit('choose', $event)" @select="selectedId = $event" />
    </div>
  </section>
</template>

<style scoped>
.location-map { display: flex; flex-direction: column; min-height: 0; flex: 1; background: radial-gradient(ellipse at 50% 45%, #2c3a28, #0e1610 78%); }
.map-bar { display: flex; justify-content: space-between; align-items: center; padding: 6px 12px; font-size: 0.7rem; letter-spacing: 0.14em; text-transform: uppercase; color: #cbbf96; }
.board-note { margin: 0; padding: 4px 12px; font-size: 0.8rem; color: #d8b68c; }
.map-layout { display: grid; grid-template-columns: minmax(0, 1fr) 300px; min-height: 0; flex: 1; }
.map-scroll { overflow: auto; min-width: 0; min-height: 340px; }
.map-canvas { position: relative; width: 100%; }
.map-links { position: absolute; inset: 0; width: 100%; height: 100%; pointer-events: none; overflow: visible; }
.map-links line { stroke: #b2996399; stroke-width: 1.2; vector-effect: non-scaling-stroke; }
.moving-enemy { fill: #eeab78; filter: drop-shadow(0 0 2px #f37045); }
.node { position: absolute; translate: -50% -50%; width: 128px; height: 88px; padding: 4px; border: 1px solid #7c8d6277; border-radius: 6px; background: #1e2b20; color: #e5e2d4; display: flex; flex-direction: column; justify-content: space-between; overflow: hidden; text-align: left; cursor: pointer; }
.node:hover { border-color: #e6c584; }
.node.hidden { border-style: dashed; }
.node.here { border-color: #afbe89; box-shadow: 0 0 0 2px #9bb98440; }
.node.selected { outline: 2px solid #e6c584; outline-offset: 2px; }
.node.targetable { box-shadow: 0 0 10px 2px #f2d27a; }
.art { position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover; object-position: 50% 28%; opacity: 0.38; z-index: 0; }
.node::after { content: ''; position: absolute; inset: 0; background: linear-gradient(#07100bd0, transparent 55%, #07100bc0); z-index: 0; pointer-events: none; }
.node > :not(.art) { position: relative; z-index: 1; }
.title { font: 0.8rem Georgia, serif; text-shadow: 0 1px 3px #000; line-height: 1.1; }
.badges, .occupants { display: flex; gap: 4px; flex-wrap: wrap; font-size: 0.7rem; }
.clue { color: #9fd3e6; }
.doom { color: #e0a595; }
.token { min-width: 18px; height: 18px; display: grid; place-items: center; border-radius: 50%; border: 1px solid currentColor; background: #09100c; font-size: 0.65rem; padding: 0 2px; }
.token.investigator { color: #c9dab1; }
.token.investigator.you { color: #ffe9a0; box-shadow: 0 0 4px #ffe9a0; }
.token.enemy { color: #d99e8e; }
.spot { position: absolute; translate: -50% -50%; width: 128px; height: 88px; border: 2px dashed #e6c584aa; border-radius: 6px; background: transparent; color: #e6c584; font-size: 1.6rem; cursor: pointer; }
@media (max-width: 800px) {
  .map-layout { grid-template-columns: minmax(0, 1fr); grid-template-rows: auto auto; }
  .node, .spot { width: 104px; height: 76px; }
}
</style>
