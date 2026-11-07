<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useElementSize } from '@vueuse/core'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Location } from '@/arkham/types/Location'
import { cardImage, portraitImage } from '@/arkham/cardImages'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import type { MapLocationInfo } from '@/arkham/composables/useLocationInfo'
import type { HemlockOverview } from '@/arkham/hemlock-overview'
import { locationLinkDirection, mineDescentPassage, type Point } from '@/arkham/locationGraph'
import LocationMarkerPlate from '@/arkham/components/LocationMarkerPlate.vue'
import LocationStandees from '@/arkham/components/LocationStandees.vue'
import { investigatorMovementName } from '@/arkham/investigatorMovement'
import { beginDrag, offsetOf } from '@/arkham/composables/useMapDrag'
import { CARD_FLIGHT_ATTR, cardFlightStyle, useCardFlightState } from '@/arkham/cardFlight'

export type MapMove = { id: string; name: string; path: string }
export type GridSpot = { label: string; x: number; y: number; index: number }
/** A room's name plate on a picture: its top-left corner and width, in percent of the picture. */
export type RoomPlateSpot = { x: number; y: number; width: number }

const props = defineProps<{
  game: Game
  playerId: string
  locations: Location[]
  positions: Record<string, Point>
  links: [string, string][]
  moves: MapMove[]
  gridSpots: GridSpot[]
  selectedId: string | null
  enemiesAt: (l: Location) => string[]
  /** What each location's plate shows; built once by the parent from the live game. */
  infoOf: (l: Location) => MapLocationInfo
  /** Where each location stands on the cutaway picture (percent); when set the map is drawn over that picture. */
  cutaway?: Record<string, Point> | null
  /** Where each room's plate goes on the cutaway picture, apart from its figures. */
  roomPlates?: Record<string, RoomPlateSpot> | null
  /** Whether the room plates show the card text and flavour. */
  plateText?: boolean
  illustratedBoard?: HemlockOverview
  /** Small version for the side column next to the scene. */
  compact?: boolean
  width?: number
  height?: number
}>()
const emit = defineEmits<{
  (e: 'inspect', location: Location, anchor: HTMLElement): void
  (e: 'choose', index: number): void
}>()

const { t } = useI18n()
const nameOf = useCardNames()
const basePos = (id: string): Point => props.cutaway?.[id] ?? props.positions[id]
const offsetKey = (id: string) => `${props.game.id}:${id}`
// Where a plate is drawn: its scenario spot plus any shift the player dragged it by (never in the house cutaway).
const pos = (id: string): Point => {
  const [x, y] = basePos(id)
  if (props.cutaway || props.compact) return [x, y]
  const [dx, dy] = offsetOf(offsetKey(id))
  return [x + dx, y + dy]
}
const arrowKey = (key: string) => `${props.game.id}:arrow:${key}`
const canDrag = computed(() => !props.cutaway && !props.compact)
const startNodeDrag = (l: Location, event: PointerEvent) => {
  if (canDrag.value) beginDrag(event, offsetKey(l.id), 'pct', stage.value)
}
const stage = ref<HTMLElement | null>(null)
const { width: stageWidth, height: stageHeight } = useElementSize(stage)
const descent = computed(() => !!props.illustratedBoard?.fixedPassages)
const canvasStyle = computed(() => {
  if (props.compact) return undefined
  const width = props.width ?? 450
  if (!props.illustratedBoard) return { minWidth: `${Math.min(width, 900)}px` }
  // An illustrated board is a picture: it always fits the available area, so it never needs scrolling.
  if (descent.value || props.illustratedBoard.village || props.illustratedBoard.house) return undefined
  const columns = Math.max(1, width / 150)
  return { '--location-width': `${87 / columns}%` }
})
// Match the card board, including enemy cards that also serve as locations.
const locationImage = (l: Location) => cardImage(l.cardCode, l.revealed || l.enemyLocation ? '' : 'b')

function hideFailedArt(event: Event, code: string) {
  const image = event.currentTarget as HTMLImageElement
  image.hidden = true
  if (import.meta.env.DEV) console.warn('[Arkham map] Unable to load art', code)
}

const connectionArrows = computed(() => {
  if (!props.illustratedBoard) return []
  const locations = new Map(props.locations.map((location) => [location.id, location]))
  return props.links.flatMap(([a, b]) => {
    const from = locations.get(a)
    const to = locations.get(b)
    if (!from || !to) return []
    const direction = locationLinkDirection(from, to)
    if (!direction) return []
    const [x1, y1] = pos(a)
    const [x2, y2] = pos(b)
    const angle = Math.atan2((y2 - y1) * stageHeight.value, (x2 - x1) * stageWidth.value)
    const moved = [a, b].some((id) => offsetOf(offsetKey(id)).some((v) => v !== 0))
    const passage = descent.value && !moved ? mineDescentPassage(from.label, to.label) : undefined
    const [x, y] = passage ?? [(x1 + x2) / 2, (y1 + y2) / 2]
    const key = `${a}:${b}`
    // The player can nudge an arrow off the spot it would sit at, if it covers something or reads badly there.
    const [dx, dy] = props.compact ? [0, 0] : offsetOf(arrowKey(key))
    return [{
      key,
      x: x + dx,
      y: y + dy,
      rotation: angle + (direction === 'backward' ? Math.PI : 0),
      both: direction === 'both',
    }]
  })
})
const ownId = () => ownInvestigator(props.game, props.playerId)?.id

/* Animation hooks, as on the card board (see LocationStandees.vue). This canvas
 * is either the overview or the scene's mini-map, never both at once, so the
 * names below have one owner. In the scene the room view is the smoke puff's
 * anchor, so the mini-map leaves that to it. */
const flightState = useCardFlightState()
const moveNameOf = (id: string, l: Location) => {
  const investigator = props.game.investigators[id]
  return investigator ? investigatorMovementName(investigator, l.id) : undefined
}
const investigatorName = (id: string) => {
  const investigator = props.game.investigators[id]
  return investigator ? nameOf(investigator.cardCode) : id
}

// A click on the tile itself (outside its plate) does what a click on the plate's name does.
function tileClick(l: Location, event: MouseEvent) {
  const info = props.infoOf(l)
  if (info.actions.selectIndex !== -1) emit('choose', info.actions.selectIndex)
  else emit('inspect', l, event.currentTarget as HTMLElement)
}
</script>

<template>
  <div class="map-scroll" :class="{ compact }">
    <div class="map-canvas" :class="{ 'has-cutaway': cutaway, 'has-illustrated-board': illustratedBoard }" :style="canvasStyle">
      <div ref="stage" class="map-stage" :class="{ cutaway, illustrated: illustratedBoard, descent, village: illustratedBoard?.village, house: illustratedBoard?.house }"
        :style="cutaway ? { backgroundImage: 'url(img/scenes/night-of-the-zealot/house-cutaway.webp)', backgroundSize: '100% 100%' } : undefined">
      <img v-if="illustratedBoard" :key="illustratedBoard.image" class="board-art" :src="illustratedBoard.image"
        alt="" @error="hideFailedArt($event, illustratedBoard.image)" />
      <svg v-if="!cutaway" class="map-links" viewBox="0 0 100 100" preserveAspectRatio="none" aria-hidden="true">
        <template v-if="!illustratedBoard">
          <line v-for="[a, b] in links" :key="a + b"
            :x1="pos(a)[0]" :y1="pos(a)[1]" :x2="pos(b)[0]" :y2="pos(b)[1]" />
        </template>
        <circle v-for="m in moves" :key="m.id" class="moving-enemy" r="1.4">
          <title>{{ m.name }}</title>
          <animateMotion dur="1.4s" :path="m.path" fill="freeze" />
        </circle>
      </svg>
      <svg v-for="arrow in connectionArrows" :key="arrow.key" class="connection-arrow" :class="{ draggable: canDrag }"
        viewBox="0 0 24 16" aria-hidden="true"
        :style="{ left: arrow.x + '%', top: arrow.y + '%', rotate: arrow.rotation + 'rad' }"
        @pointerdown="canDrag && beginDrag($event, arrowKey(arrow.key), 'pct', stage)">
        <path d="M4 8h16m-5-5 5 5-5 5" />
        <path v-if="arrow.both" d="m9 3-5 5 5 5" />
      </svg>
      <button v-for="spot in gridSpots" :key="spot.label" class="spot"
        :style="{ left: spot.x + '%', top: spot.y + '%' }"
        :aria-label="t('locationMap.placeHere')" @click="emit('choose', spot.index)">+</button>
      <div v-for="l in locations" :key="l.id" class="node"
        :class="{ draggable: canDrag,
          here: infoOf(l).here, selected: l.id === selectedId,
          hidden: !l.revealed, targetable: infoOf(l).targetable,
        }"
        :style="{ left: pos(l.id)[0] + '%', top: pos(l.id)[1] + '%', zIndex: cutaway ? Math.round(pos(l.id)[1]) : undefined }"
        :data-id="l.id" @pointerdown="startNodeDrag(l, $event)" @click.stop="cutaway ? undefined : tileClick(l, $event)">
        <img v-if="!cutaway && !illustratedBoard" class="art" :src="locationImage(l)" alt="" loading="lazy" />
        <LocationStandees v-if="cutaway" :game="game" :playerId="playerId" :location="l" :enemiesAt="enemiesAt" />
        <LocationMarkerPlate v-if="!roomPlates?.[l.id]" :info="infoOf(l)" :size="compact ? 'compact' : 'full'"
          :[CARD_FLIGHT_ATTR]="l.cardId" :style="cardFlightStyle(flightState, l.cardId)"
          :data-image="locationImage(l)" :data-card-code="l.cardCode"
          @inspect="emit('inspect', l, $event)" @choose="emit('choose', $event)" />
        <div v-if="illustratedBoard" class="location-floor">
          <LocationStandees :game="game" :playerId="playerId" :location="l" :enemiesAt="enemiesAt" />
        </div>
        <span v-else-if="!cutaway" class="occupants">
          <img v-for="id in l.investigators" :key="id" class="figure" :class="{ you: id === ownId(), portrait: !compact }"
            :src="portraitImage(game.investigators[id]?.cardCode ?? '')" :alt="investigatorName(id)" :title="investigatorName(id)"
            :style="{ viewTransitionName: moveNameOf(id, l) }" :data-investigator-mini="compact ? undefined : id"
            :data-image="cardImage(game.investigators[id]?.cardCode ?? '')" />
          <span v-if="enemiesAt(l).length" class="token enemy">☠{{ enemiesAt(l).length > 1 ? ' ' + enemiesAt(l).length : '' }}</span>
        </span>
      </div>
      <template v-for="l in locations" :key="'plate-' + l.id">
        <div v-if="roomPlates?.[l.id]" class="room-plate" :class="{ selected: l.id === selectedId }" :data-id="l.id"
          :style="{ left: roomPlates[l.id].x + '%', top: roomPlates[l.id].y + '%', width: roomPlates[l.id].width + '%' }">
          <LocationMarkerPlate :info="infoOf(l)" size="room" :showText="plateText"
            :[CARD_FLIGHT_ATTR]="l.cardId" :style="cardFlightStyle(flightState, l.cardId)"
            :data-image="locationImage(l)" :data-card-code="l.cardCode" @inspect="emit('inspect', l, $event)" @choose="emit('choose', $event)" />
        </div>
      </template>
      </div>
    </div>
  </div>
</template>

<style scoped>
.map-scroll { overflow: auto; min-width: 0; min-height: 0; display: flex; }
.map-canvas { position: relative; width: 100%; flex: 1; min-height: 240px; }
.compact .map-canvas { height: 100%; min-height: 180px; aspect-ratio: 4 / 3; }
.map-stage { position: absolute; inset: 0; }
.has-illustrated-board { min-height: 0; }
.map-scroll:has(.has-illustrated-board) { overflow: hidden; }
.board-art { position: absolute; inset: 0; width: 100%; height: 100%; object-fit: fill; pointer-events: none; }
.has-cutaway { container-type: size; }
.map-stage.cutaway { inset: auto; left: 50%; top: 50%; translate: -50% -50%; width: min(100cqw, 177.7cqh); aspect-ratio: 1672 / 941; border-radius: 6px; box-shadow: 0 0 40px #000a; }
/* On the house the room is the picture: the people stand on its floor, its plate sits in a corner of it. */
.cutaway .node { translate: -50% -100%; width: auto; height: auto; min-height: 0; padding: 0; background: transparent; border: 0; overflow: visible; align-items: center; justify-content: flex-end; gap: 3px; cursor: default; pointer-events: none; }
.cutaway .node::after { display: none; }
.room-plate { position: absolute; z-index: 110; }
.room-plate.selected :deep(.plate) { outline: 2px solid #e6c584; outline-offset: 2px; }
.map-links { position: absolute; inset: 0; width: 100%; height: 100%; pointer-events: none; overflow: visible; }
.map-links line { stroke: #b2996399; stroke-width: 1.2; vector-effect: non-scaling-stroke; }
.moving-enemy { fill: #eeab78; filter: drop-shadow(0 0 2px #f37045); }
.node { position: absolute; translate: -50% -50%; width: clamp(140px, 19%, 240px); height: auto; min-height: clamp(96px, 27%, 170px); padding: 5px; border: 1px solid #7c8d6277; border-radius: 6px; background: #1e2b20; color: #e5e2d4; display: flex; flex-direction: column; justify-content: space-between; overflow: hidden; text-align: left; cursor: pointer; }
.node:hover { border-color: #e6c584; }
.node.draggable { touch-action: none; }
.node.draggable :deep(.plate) { cursor: grab; }
.node.hidden { border-style: dashed; }
.node.here { border-color: #afbe89; box-shadow: 0 0 0 2px #9bb98440; }
.node.selected { outline: 2px solid #e6c584; outline-offset: 2px; }
.node.targetable { box-shadow: 0 0 10px 2px #f2d27a; }
.map-stage:not(.cutaway) .node :deep(.plate) { max-width: 100%; }
.art { position: absolute; inset: 0; width: 100%; height: 100%; object-fit: cover; object-position: 50% 28%; opacity: 0.75; z-index: 0; }
.node::after { content: ''; position: absolute; inset: 0; background: linear-gradient(transparent 45%, #07100bc0); z-index: 0; pointer-events: none; }
.node > :not(.art) { position: relative; z-index: 1; }
.occupants { display: flex; gap: 4px; flex-wrap: wrap; font-size: 0.85rem; font-weight: 700; text-shadow: 0 1px 3px #000; }
.figure { width: clamp(26px, 3vw, 40px); aspect-ratio: 1; border-radius: 50%; object-fit: cover; object-position: 50% 20%; border: 2px solid #c9dab1; background: #09100c; }
.figure.you { border-color: #ffe9a0; box-shadow: 0 0 8px #ffe9a0; }
.token { min-width: 18px; height: 18px; display: grid; place-items: center; border-radius: 50%; border: 1px solid currentColor; background: #09100c; font-size: 0.65rem; padding: 0 2px; }
.token.enemy { color: #d99e8e; }
.spot { position: absolute; translate: -50% -50%; width: clamp(140px, 19%, 240px); height: clamp(96px, 27%, 170px); border: 2px dashed #e6c584aa; border-radius: 6px; background: transparent; color: #e6c584; font-size: 1.6rem; cursor: pointer; }
.compact .node, .compact .spot { width: 64px; height: 42px; min-height: 0; padding: 2px; }
.compact .figure { width: 14px; border-width: 1px; }
.compact .occupants { font-size: 0.55rem; gap: 2px; }
.compact .token { min-width: 12px; height: 12px; font-size: 0.5rem; }
@media (max-width: 800px) {
  .node { width: 128px; min-height: 96px; }
  .spot { width: 128px; height: 96px; }
  .compact .node, .compact .spot { width: 64px; height: 42px; min-height: 0; }
}
.illustrated .node, .illustrated .spot { width: min(var(--location-width, 20%), 310px); min-height: 0; box-sizing: border-box; }
.illustrated .node { overflow: visible; padding: 0; background: #101912d9; }
.illustrated .node::after { display: none; }
.illustrated .node :deep(.plate) { min-width: 0; }
.illustrated .node :deep(.plate .name) { white-space: normal; overflow-wrap: anywhere; }
.illustrated .spot { height: 100px; }
.location-floor { position: absolute; top: 100%; left: 0; width: 100%; isolation: isolate; }
.location-illustration {
  display: block; position: relative; overflow: hidden; aspect-ratio: 1.9;
  max-height: 128px; border-radius: 0 0 6px 6px;
}
.location-illustration img { position: absolute; top: 0; left: 0; width: 100%; translate: 0 -12%; }
.location-floor :deep(.standees) {
  /* In flow, so the floor grows to hold everyone with their bars; the floor hangs below the plate, never over it. */
  position: relative; justify-content: center; flex-wrap: wrap; gap: 2px 4px; padding-top: 4px;
}
/* Health, sanity, resources and clues stay over every head on the illustrated boards too, just tighter. */
.location-floor :deep(.hud) { min-width: 0; padding: 2px 3px; font-size: 9px; }
.location-floor :deep(.hud .track) { width: 16px; }
.location-floor :deep(.standees) { z-index: 2; }
.location-floor :deep(.figure) { height: calc(64px * var(--figure-scale, 1)); max-width: none; image-rendering: auto; }
.location-floor :deep(.crop) { width: calc(48px * var(--figure-scale, 1)); }
.connection-arrow.draggable { pointer-events: auto; cursor: grab; touch-action: none; }
.connection-arrow {
  position: absolute;
  box-sizing: border-box;
  width: 38px;
  height: 30px;
  padding: 5px 6px;
  translate: -50% -50%;
  pointer-events: none;
  /* A dark badge with a bright rim so the arrow reads on any part of the painted map. */
  background: #140f09e6;
  border: 2px solid #f6dc9c;
  border-radius: 999px;
  box-shadow: 0 0 0 1px #000a, 0 2px 8px #000c, 0 0 10px #f2d27a66;
  fill: none;
  stroke: #ffe9a8;
  stroke-width: 2.6;
  stroke-linecap: round;
  stroke-linejoin: round;
}
.descent .node {
  width: min(25%, 240px);
  height: auto;
  min-height: 0;
  padding: 0;
  border: 0;
  background: transparent;
  overflow: visible;
  box-shadow: none;
  cursor: default;
  pointer-events: none;
}
.descent .node::after { display: none; }
.descent .node.selected { outline: none; }
.descent .node.selected :deep(.plate) { outline: 2px solid #e6c584; outline-offset: 2px; }
.descent .node :deep(.plate) { box-sizing: border-box; width: 100%; min-width: 0; }
.descent .node :deep(.plate .name) { white-space: normal; overflow-wrap: anywhere; }
.descent .location-floor { width: 100%; }
.village .node {
  width: min(20%, 195px); background: transparent; border: 0; box-shadow: none;
  pointer-events: none;
}
.village .node.selected { outline: none; }
.village .node.selected :deep(.plate) { outline: 2px solid #e6c584; outline-offset: 2px; }
.house .node, .house .spot { width: min(22%, 180px); }
/* Player-chosen sizes (Settings): location buttons/plates and character figures. */
.node, .spot, .room-plate { scale: var(--location-scale, 1); }
.room-plate { transform-origin: 0 0; }
.cutaway .node { scale: 1; transform-origin: 50% 100%; }
.cutaway .node :deep(.standees) { scale: var(--figure-scale, 1); transform-origin: 50% 100%; }
.figure { zoom: var(--figure-scale, 1); }
.house .location-illustration { max-height: 75px; }
/* On painted boards a plate is only as wide as its name, stats and info icon need, so it covers as little of the picture as possible. */
.illustrated .node, .village .node, .descent .node, .house .node { width: max-content; max-width: min(var(--location-width, 22%), 240px); }
.illustrated .node :deep(.plate), .village .node :deep(.plate), .descent .node :deep(.plate), .house .node :deep(.plate) { min-width: 0; padding: 2px 5px 3px; gap: 2px; }
.illustrated .node :deep(.plate .main), .village .node :deep(.plate .main), .descent .node :deep(.plate .main), .house .node :deep(.plate .main) { min-height: 18px; gap: 4px; }
.illustrated .node :deep(.plate .name), .village .node :deep(.plate .name), .descent .node :deep(.plate .name), .house .node :deep(.plate .name) { font-size: 0.82rem; }
</style>
