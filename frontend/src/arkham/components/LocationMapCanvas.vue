<script lang="ts" setup>
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Message } from '@/arkham/types/Message'
import type { Location } from '@/arkham/types/Location'
import { TokenType } from '@/arkham/types/Token'
import { cardImage } from '@/arkham/cardImages'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { selectLocationIndex } from '@/arkham/locationChoices'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import type { Point } from '@/arkham/locationGraph'

export type MapMove = { id: string; name: string; path: string }
export type GridSpot = { label: string; x: number; y: number; index: number }

const props = defineProps<{
  game: Game
  playerId: string
  locations: Location[]
  positions: Record<string, Point>
  links: [string, string][]
  moves: MapMove[]
  gridSpots: GridSpot[]
  selectedId: string | null
  myLocation: string | null
  choices: readonly Message[]
  enemiesAt: (l: Location) => string[]
  /** Small version for the side column next to the scene. */
  compact?: boolean
  width?: number
  height?: number
}>()
const emit = defineEmits<{ (e: 'activate', location: Location): void; (e: 'choose', index: number): void }>()

const { t } = useI18n()
const nameOf = useCardNames()
const ownId = () => ownInvestigator(props.game, props.playerId)?.id
const clueCount = (l: Location) => l.tokens[TokenType.Clue] ?? 0
const doomCount = (l: Location) => l.tokens[TokenType.Doom] ?? 0
const investigatorName = (id: string) => {
  const investigator = props.game.investigators[id]
  return investigator ? nameOf(investigator.cardCode) : id
}
</script>

<template>
  <div class="map-scroll" :class="{ compact }">
    <div class="map-canvas" :style="compact ? undefined : { minWidth: (width ?? 450) + 'px', height: (height ?? 360) + 'px' }">
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
          here: l.id === myLocation, selected: l.id === selectedId,
          hidden: !l.revealed, targetable: selectLocationIndex(choices, l) !== -1,
        }"
        :style="{ left: positions[l.id][0] + '%', top: positions[l.id][1] + '%' }"
        :data-id="l.id" @click="emit('activate', l)">
        <img class="art" :src="cardImage(l.cardCode, l.revealed ? '' : 'b')" alt="" loading="lazy" />
        <span class="title">{{ nameOf(l.cardCode) }}</span>
        <span class="badges">
          <span v-if="clueCount(l)" class="clue" :title="t('locationMap.clues')">◇{{ clueCount(l) }}</span>
          <span v-if="doomCount(l)" class="doom" :title="t('locationMap.doom')">☠{{ doomCount(l) }}</span>
        </span>
        <span class="occupants">
          <span v-for="id in l.investigators" :key="id" class="token investigator" :class="{ you: id === ownId() }"
            :title="investigatorName(id)">{{ investigatorName(id).charAt(0) }}</span>
          <span v-if="enemiesAt(l).length" class="token enemy">☠{{ enemiesAt(l).length > 1 ? ' ' + enemiesAt(l).length : '' }}</span>
        </span>
      </button>
    </div>
  </div>
</template>

<style scoped>
.map-scroll { overflow: auto; min-width: 0; min-height: 0; }
.map-canvas { position: relative; width: 100%; }
.compact .map-canvas { height: 100%; min-height: 180px; aspect-ratio: 4 / 3; }
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
.compact .node, .compact .spot { width: 58px; height: 40px; padding: 2px; }
.compact .title { font-size: 0.55rem; }
.compact .badges, .compact .occupants { font-size: 0.55rem; gap: 2px; }
.compact .token { min-width: 12px; height: 12px; font-size: 0.5rem; }
@media (max-width: 800px) {
  .node, .spot { width: 104px; height: 76px; }
  .compact .node, .compact .spot { width: 58px; height: 40px; }
}
</style>
