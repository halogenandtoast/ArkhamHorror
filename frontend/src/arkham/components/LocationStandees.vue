<script lang="ts" setup>
import { computed } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Location } from '@/arkham/types/Location'
import { TokenType } from '@/arkham/types/Token'
import { cardArt, cardImage, investigatorCardArt, portraitImage } from '@/arkham/cardImages'
import { useCardNames } from '@/arkham/composables/useCardNames'
import { spriteFor } from '@/arkham/sceneSprites'
import { resolveCalculation } from '@/arkham/types/Calculation'
import { ownInvestigator } from '@/arkham/ownInvestigator'
import { useCardStore } from '@/stores/cards'
import { investigatorMovementName } from '@/arkham/investigatorMovement'
import { beginDrag, offsetOf } from '@/arkham/composables/useMapDrag'
import { CARD_FLIGHT_ATTR, cardFlightStyle, useCardFlightState } from '@/arkham/cardFlight'

/* Everyone standing in a room of the cutaway house, investigators first, each with the numbers shown above
 * their head: health and sanity bars, resources and clues for investigators, health for enemies.
 *
 * The figures also carry the board's animation hooks, the same ones the card
 * board puts on its minis and enemy cards, so moving, spawning and vanishing
 * animate here too:
 * - `moveName`: the view-transition name Game.vue's movement transition looks
 *   for (`investigator-<id>`, `enemy-<id>`). An engaged enemy gets none: its
 *   card in the threat area owns that name, and two would abort the transition.
 * - `cardId` on an enemy's picture: where a revealed card lands (cardFlight.ts).
 *   On the picture rather than the figure, which already holds the movement name.
 * - `data-investigator-mini` + `.portrait`: the smoke puff's anchor. */
const props = defineProps<{ game: Game; playerId: string; location: Location; enemiesAt: (l: Location) => string[] }>()

const nameOf = useCardNames()
const cardStore = useCardStore()
const flightState = useCardFlightState()

type Meter = { left: number; max: number }
type Fig = {
  id: string; kind: 'investigator' | 'enemy' | 'asset'; tone: string; name: string; src: string | null; art: string; you: boolean; exhausted: boolean; note: string | null
  moveName: string | undefined; cardId: string | undefined
  /* The whole card, for the card overlay on hover. */
  cardImage: string
  health: Meter | null; sanity: Meter | null; resources: number | null; clues: number | null
}
const meter = (max: number, taken: number): Meter => ({ max, left: Math.max(0, max - taken) })
const pct = (m: Meter) => (100 * m.left / Math.max(1, m.max)) + '%'

const figures = computed<Fig[]>(() => {
  const l = props.location
  const mine = ownInvestigator(props.game, props.playerId)?.id
  const people = l.investigators.flatMap((id): Fig[] => {
    const i = props.game.investigators[id]
    if (!i) return []
    const art = investigatorCardArt(i)
    const src = spriteFor(art) ?? (i.form.tag === 'RegularForm' ? portraitImage(i.cardCode) : null)
    return [{
      id, kind: 'investigator', tone: (i.class ?? 'Neutral').toLowerCase(), name: nameOf(i.cardCode), src, art: cardImage(art), you: id === mine, exhausted: false, note: null,
      moveName: investigatorMovementName(i, l.id), cardId: undefined, cardImage: cardImage(art),
      health: meter(i.health, i.tokens[TokenType.Damage] ?? 0), sanity: meter(i.sanity, i.tokens[TokenType.Horror] ?? 0),
      resources: i.tokens[TokenType.Resource] ?? 0, clues: i.tokens[TokenType.Clue] ?? 0,
    }]
  })
  const monsters = props.enemiesAt(l).slice(0, 4).flatMap((id): Fig[] => {
    const e = props.game.enemies[id]
    if (!e) return []
    const damage = (e.tokens[TokenType.Damage] ?? 0) + e.assignedDamage
    const max = resolveCalculation(e.health, Object.keys(props.game.investigators).length)
    const definition = cardStore.cards.find((card) => card.cardCode === e.cardCode)
    const art = cardArt(definition?.art ?? e.cardCode, e.flipped ? 'b' : '')
    return [{
      id, kind: 'enemy', tone: 'enemy', name: nameOf(e.cardCode), src: spriteFor(art), art: cardImage(art), you: false, exhausted: e.exhausted,
      moveName: e.placement.tag === 'AtLocation' ? `enemy-${id}` : undefined,
      cardId: e.placement.tag === 'AtLocation' ? e.cardId : undefined, cardImage: cardImage(art),
      note: max === null ? `♥ ${damage} dmg` : null,
      health: max === null ? null : meter(max, damage), sanity: null, resources: null, clues: null,
    }]
  })
  /* Character assets (residents, allies) standing at the location: anything with health or sanity printed. */
  const residents = l.assets.flatMap((id): Fig[] => {
    const a = props.game.assets[id]
    if (!a || (a.health === null && a.sanity === null)) return []
    const definition = cardStore.cards.find((card) => card.cardCode === a.cardCode)
    const art = cardArt(definition?.art ?? a.cardCode, a.flipped ? 'b' : '')
    return [{
      id, kind: 'asset', tone: 'asset', name: nameOf(a.cardCode), src: spriteFor(art), art: cardImage(art), you: false, exhausted: a.exhausted,
      moveName: undefined, cardId: a.cardId, cardImage: cardImage(art), note: null,
      health: a.health === null ? null : meter(a.health, (a.tokens[TokenType.Damage] ?? 0) + a.assignedHealthDamage),
      sanity: a.sanity === null ? null : meter(a.sanity, (a.tokens[TokenType.Horror] ?? 0) + a.assignedSanityDamage),
      resources: null, clues: null,
    }]
  })
  return [...people, ...residents, ...monsters]
})
// A figure can be dragged to a spot the player finds easier to follow; it still belongs to this location.
const figKey = (id: string) => `${props.game.id}:${props.location.id}:${id}`
const figShift = (id: string) => { const [x, y] = offsetOf(figKey(id)); return x || y ? `${x}px ${y}px` : undefined }
const startFigDrag = (id: string, event: PointerEvent) => {
  event.stopPropagation()
  beginDrag(event, figKey(id), 'px', null)
}
import GameIcon from '@/arkham/components/GameIcon.vue'
</script>

<template>
  <span class="standees">
    <span v-for="f in figures" :key="f.id" class="fig" :class="[f.kind, f.tone, { you: f.you, exhausted: f.exhausted }]" :title="f.name"
      :style="{ viewTransitionName: f.moveName, translate: figShift(f.id) }" @pointerdown="startFigDrag(f.id, $event)" :data-investigator-mini="f.kind === 'investigator' ? f.id : undefined" :data-fx-target="`${f.kind}-${f.id}`"
      :data-image="f.cardImage">
      <span v-if="f.health || f.sanity" class="hud">
        <span v-if="f.health" class="meter hp"><GameIcon type="health" /><span class="track"><i :style="{ width: pct(f.health) }"></i></span><b>{{ f.health.left }}/{{ f.health.max }}</b></span>
        <span v-if="f.sanity" class="meter sanity"><GameIcon type="sanity" /><span class="track"><i :style="{ width: pct(f.sanity) }"></i></span><b>{{ f.sanity.left }}/{{ f.sanity.max }}</b></span>
        <span v-if="f.resources !== null" class="counts"><span class="count res"><GameIcon type="resource" /> {{ f.resources }}</span><span class="count clue"><GameIcon type="clue" /> {{ f.clues }}</span></span>
      </span>
      <span v-else-if="f.note" class="hud"><span class="meter hp">{{ f.note }}</span></span>
      <img v-if="f.src" class="figure portrait" :src="f.src" :alt="f.name" draggable="false"
        :[CARD_FLIGHT_ATTR]="f.cardId" :style="cardFlightStyle(flightState, f.cardId)" />
      <span v-else class="crop portrait" role="img" :aria-label="f.name"
        :[CARD_FLIGHT_ATTR]="f.cardId" :style="[{ backgroundImage: `url(${f.art})` }, cardFlightStyle(flightState, f.cardId)]"></span>
    </span>
  </span>
</template>

<style scoped>
.standees { display: flex; align-items: flex-end; gap: 6px; flex-wrap: nowrap; filter: drop-shadow(2px 4px 3px #000c); pointer-events: none; }
/* The row lets clicks through to the room; the figures themselves take the pointer so hovering one opens its card. */
.fig { display: grid; justify-items: center; gap: 2px; pointer-events: auto; cursor: grab; touch-action: none; position: relative; z-index: 1; }
.figure { width: auto; height: clamp(46px, 11cqh, 110px); object-fit: contain; object-position: bottom; }
/* Each figure gets a crisp outline instead of a glow: an investigator's in their class colour (Guardian blue,
 * Seeker orange, ...), enemies' in red. Four hard, unblurred shadows trace the sprite's silhouette. */
.fig { --outline: #d6c48a; }
.fig.enemy { --outline: #ff6a1f; }
.fig.asset { --outline: #7fb08a; }
.fig.guardian { --outline: var(--guardian, #2f7fd8); }
.fig.seeker { --outline: var(--seeker, #ec8a24); }
.fig.rogue { --outline: var(--rogue, #2fa05a); }
.fig.mystic { --outline: var(--mystic, #8a52c9); }
.fig.survivor { --outline: #a3141f; }
.fig.neutral { --outline: var(--neutral, #9aa0a6); }
.fig .figure {
  filter: drop-shadow(2px 0 0 var(--outline)) drop-shadow(-2px 0 0 var(--outline)) drop-shadow(0 2px 0 var(--outline)) drop-shadow(0 -2px 0 var(--outline));
}
.fig.you .figure { filter: drop-shadow(2px 0 0 var(--outline)) drop-shadow(-2px 0 0 var(--outline)) drop-shadow(0 2px 0 var(--outline)) drop-shadow(0 -2px 0 var(--outline)) drop-shadow(0 0 5px #fff8); }
.fig .crop { border-color: var(--outline); border-width: 3px; }
.fig.exhausted .figure { opacity: 0.65; rotate: 7deg; }
.crop { display: block; width: clamp(40px, 8cqh, 72px); aspect-ratio: 1; border-radius: 50%; border: 2px solid #d99e8e; background: #1a2620 no-repeat 50% 17% / 165% auto; }
.hud { display: grid; gap: 2px; min-width: 56px; padding: 3px 5px; background: #111815ef; border: 1px solid #9f8d605c; border-radius: 4px; font: 600 10px/1.15 system-ui, sans-serif; white-space: nowrap; text-shadow: none; }
.hud .meter { display: flex; align-items: center; gap: 3px; font-variant-numeric: tabular-nums; }
.hud .meter.hp { color: #ff9b87; }
.hud .meter.sanity { color: #9fceee; }
.hud .track { width: 22px; height: 4px; background: #67645d; border-radius: 3px; overflow: hidden; }
.hud .track i { display: block; height: 100%; background: currentColor; }
.hud .meter b { margin-left: auto; color: #fff4df; }
.hud .counts { display: flex; justify-content: space-between; gap: 8px; border-top: 1px solid #ffffff1a; padding-top: 2px; }
.hud .count.res { color: #f0d9a8; }
.hud .count.clue { color: #9fd3e6; }
</style>
