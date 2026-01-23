<script lang="ts" setup>
import { computed } from 'vue'
import { TokenType } from '@/arkham/types/Token'
import { keyToId } from '@/arkham/types/Key'
import PoolItem from '@/arkham/components/PoolItem.vue'
import KeyToken from '@/arkham/components/Key.vue'
import Seal from '@/arkham/components/Seal.vue'
import type { Message } from '@/arkham/types/Message'
import * as Arkham from '@/arkham/types/Investigator'
import type { Game } from '@/arkham/types/Game'
import { useDebug } from '@/arkham/debug'
import { MessageType } from '@/arkham/types/Message'

const emits = defineEmits<{
  choose: [value: number]
}>()

export interface Props {
  choices: Message[]
  investigator: Arkham.Investigator
  game: Game
  portrait?: boolean
  playerId: string
}

const props = withDefaults(defineProps<Props>(), { portrait: false })
const debug = useDebug()

const iid = computed(() => props.investigator.id)
const choose = (index: number) => emits('choose', index)

function findInvestigatorComponentIndex(tokenType: "DamageToken" | "HorrorToken", tag: MessageType) {
  return computed(() =>
    props.choices.findIndex((c) =>
      c.tag === tag
      && c.component.tag === "InvestigatorComponent"
      && c.component.tokenType === tokenType
      && c.component.investigatorId === iid.value
    )
  )
}

const healthAction = findInvestigatorComponentIndex("DamageToken", MessageType.COMPONENT_LABEL)
const sanityAction = findInvestigatorComponentIndex("HorrorToken", MessageType.COMPONENT_LABEL)
const sanityAuxAction = findInvestigatorComponentIndex("HorrorToken", MessageType.AUXILIARY_COMPONENT_LABEL)

const takeResourceAction = computed(() =>
  props.choices.findIndex((c) =>
    c.tag === MessageType.COMPONENT_LABEL
    && c.component.tag === "InvestigatorComponent"
    && c.component.tokenType === "ResourceToken"
    && c.component.investigatorId === iid.value
  )
)

const spendCluesAction = computed(() =>
  props.choices.findIndex((c) =>
    c.tag === MessageType.COMPONENT_LABEL
    && c.component.tag === "InvestigatorComponent"
    && c.component.tokenType === "ClueToken"
    && c.component.investigatorId === iid.value
  )
)

const keys = computed(() => props.investigator.keys)
const seals = computed(() => props.investigator.seals)
const doom = computed(() => props.investigator.tokens[TokenType.Doom])
const clues = computed(() => props.investigator.tokens[TokenType.Clue] || 0)
const resources = computed(() => props.investigator.tokens[TokenType.Resource] || 0)
const horror = computed(() => (props.investigator.tokens[TokenType.Horror] || 0) + props.investigator.assignedSanityDamage - props.investigator.assignedSanityHeal)
const damage = computed(() => (props.investigator.tokens[TokenType.Damage] || 0) + props.investigator.assignedHealthDamage - props.investigator.assignedHealthHeal)
const alarmLevel = computed(() => props.investigator.tokens[TokenType.AlarmLevel] || 0)
const leylines = computed(() => props.investigator.tokens[TokenType.Leyline] || 0)

function onSanityClickCapture(e: MouseEvent) {
  if (!e.shiftKey) return
  const idx = sanityAuxAction.value
  if (idx === -1) return
  e.preventDefault()
  e.stopPropagation()
  choose(idx)
}

const showSanityAux = computed(() => sanityAuxAction.value !== -1 && sanityAction.value !== -1)

function polar(cx: number, cy: number, r: number, deg: number) {
  const rad = (deg * Math.PI) / 180
  return { x: cx + r * Math.cos(rad), y: cy + r * Math.sin(rad) }
}

function arcPath(cx: number, cy: number, r: number, startDeg: number, endDeg: number) {
  let s = startDeg
  let e = endDeg
  if (e < s) e += 360
  const p0 = polar(cx, cy, r, s)
  const p1 = polar(cx, cy, r, e)
  const large = e - s > 180 ? 1 : 0
  return `M ${p0.x} ${p0.y} A ${r} ${r} 0 ${large} 1 ${p1.x} ${p1.y}`
}

function annularSectorPath(cx: number, cy: number, rInner: number, rOuter: number, startDeg: number, endDeg: number) {
  let s = startDeg
  let e = endDeg
  if (e < s) e += 360

  const p0 = polar(cx, cy, rOuter, s)
  const p1 = polar(cx, cy, rOuter, e)
  const p2 = polar(cx, cy, rInner, e)
  const p3 = polar(cx, cy, rInner, s)

  const large = e - s > 180 ? 1 : 0

  return [
    `M ${p0.x} ${p0.y}`,
    `A ${rOuter} ${rOuter} 0 ${large} 1 ${p1.x} ${p1.y}`,
    `L ${p2.x} ${p2.y}`,
    `A ${rInner} ${rInner} 0 ${large} 0 ${p3.x} ${p3.y}`,
    `Z`,
  ].join(' ')
}

const auxSectorD = computed(() => annularSectorPath(50, 50, 30, 48, 285, 345))
const auxLabelArcD = computed(() => arcPath(50, 50, 39, 287, 343))
const labelArcId = computed(() => `auxLabelArc-${iid.value}`)
const hiGradId = computed(() => `auxMagentaHi-${iid.value}`)
</script>

<template>
  <div class="resources">
    <div class="keys" v-if="keys.length > 0">
      <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" @choose="choose" />
    </div>

    <div class="seals" v-if="seals.length > 0">
      <Seal v-for="seal in seals" :key="seal.sealKind" :seal="seal" />
    </div>

    <template v-if="debug.active">
      <button
        @click.exact="debug.send(game.id, {tag: 'LoseResources', contents: [iid, {tag: 'GameSource'}, 1]})"
        @click.shift="debug.send(game.id, {tag: 'LoseAllResources', contents: [iid, {tag: 'GameSource'}]})"
      >-</button>
    </template>

    <PoolItem
      type="resource"
      :amount="resources"
      :class="{ 'resource--can-take': takeResourceAction !== -1 }"
      @choose="$emit('choose', takeResourceAction)"
    />

    <template v-if="debug.active">
      <button
        class="plus-button"
        @click.exact="debug.send(game.id, {tag: 'TakeResources', contents: [iid, 1, {tag: 'GameSource' }, false]})"
        @click.shift="debug.send(game.id, {tag: 'TakeResources', contents: [iid, 5, {tag: 'GameSource' }, false]})"
      >+</button>
    </template>

    <template v-if="debug.active">
      <button
        @click.exact="debug.send(game.id, {tag: 'GainClues', contents: [iid, {tag: 'GameSource' }, -1]})"
        @click.shift="debug.send(game.id, {tag: 'InvestigatorDiscardAllClues', contents: [{tag: 'GameSource' }, iid]})"
      >-</button>
    </template>

    <PoolItem
      type="clue"
      :amount="clues"
      :class="{ 'clue--can-spend': spendCluesAction !== -1 }"
      @choose="$emit('choose', spendCluesAction)"
    />

    <template v-if="debug.active">
      <button
        class="plus-button"
        @click.exact="debug.send(game.id, {tag: 'GainClues', contents: [iid, {tag: 'GameSource' }, 1]})"
        @click.shift="debug.send(game.id, {tag: 'GainClues', contents: [iid, {tag: 'GameSource' }, 5]})"
      >+</button>
    </template>

    <template v-if="debug.active">
      <button
        @click.exact="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: iid}, {tag: 'TestSource', contents: []}, 1]})"
        @click.shift="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: iid}, {tag: 'TestSource', contents: []}, investigator.tokens['Damage'] ?? 0]})"
      >-</button>
    </template>

    <PoolItem
      type="health"
      :amount="damage"
      :class="{ 'health--can-interact': healthAction !== -1 }"
      @choose="$emit('choose', healthAction)"
    />

    <template v-if="debug.active">
      <button
        class="plus-button"
        @click.exact="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [iid, {tag: 'TestSource', contents: []}, 1, 0]})"
        @click.shift="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [iid, {tag: 'TestSource', contents: []}, 5, 0]})"
      >+</button>
    </template>

    <template v-if="debug.active">
      <button
        @click.exact="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: iid}, {tag: 'TestSource', contents: []}, 1]})"
        @click.shift="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: iid}, {tag: 'TestSource', contents: []}, investigator.tokens['Horror'] ?? 0]})"
      >-</button>
    </template>

    <div
      class="pool-item-wrap pool-item-wrap--sanity"
      :class="{ 'pool-item-wrap--active': sanityAction !== -1 }"
      @click.capture="onSanityClickCapture"
    >
      <PoolItem
        type="sanity"
        :amount="horror"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="choose(sanityAction)"
      />

      <div
        v-if="showSanityAux"
        class="aux-arc"
        role="button"
        tabindex="0"
        aria-label="Assign all horror"
        @keydown.enter.prevent="choose(sanityAuxAction)"
        @keydown.space.prevent="choose(sanityAuxAction)"
      >
        <svg class="aux-arc__svg" viewBox="0 0 100 100" aria-hidden="true">
          <defs>
            <path :id="labelArcId" :d="auxLabelArcD" />
            <radialGradient :id="hiGradId" cx="30%" cy="25%" r="75%">
              <stop offset="0%" stop-color="#ffffff" stop-opacity="0.22" />
              <stop offset="35%" stop-color="#ffffff" stop-opacity="0.10" />
              <stop offset="70%" stop-color="#ffffff" stop-opacity="0.00" />
            </radialGradient>
          </defs>

          <path
            class="aux-arc__base aux-arc__hit"
            :d="auxSectorD"
            @click.stop.prevent="choose(sanityAuxAction)"
          />

          <path class="aux-arc__hi" :d="auxSectorD" :fill="`url(#${hiGradId})`" />

          <text class="aux-arc__labelText">
            <textPath
              :href="`#${labelArcId}`"
              startOffset="50%"
              text-anchor="middle"
              method="align"
              spacing="auto"
            >
            <tspan dy="0.35em">{{ $t('applyAll') }}</tspan>
            </textPath>
          </text>
        </svg>
      </div>
    </div>

    <template v-if="debug.active">
      <button
        class="plus-button"
        @click.exact="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [iid, {tag: 'TestSource', contents: []}, 0, 1]})"
        @click.shift="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [iid, {tag: 'TestSource', contents: []}, 0, 5]})"
      >+</button>
    </template>

    <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />

    <PoolItem
      v-if="alarmLevel > 0"
      type="doom"
      :amount="alarmLevel"
      tooltip="Alarm Level"
    />

    <PoolItem
      v-if="leylines > 0"
      type="resource"
      :amount="leylines"
      tooltip="Leyline"
    />
  </div>
</template>

<style scoped>
.resources {
  display: flex;
  justify-content: space-between;
}
.resources button {
  height: min-content;
  align-self: center;
}

@media (max-width: 800px) and (orientation: portrait) {
  .resources {
    gap: 8px;
    margin-top: auto;
    width: fit-content;
  }
  .resources :deep(.poolItem) {
    width: calc(var(--pool-token-width) * 1.2);
  }
}

.plus-button {
  margin-right: 10px;
}

.pool-item-wrap {
  position: relative;
  display: inline-flex;
  overflow: visible;
}

.pool-item-wrap--sanity {
  --ring: var(--select);
  --ringSoft: rgba(210, 80, 255, 0.28);
}

/* Larger hover zone so moving through the gap doesn't collapse the control */
.pool-item-wrap--active.pool-item-wrap--sanity::before {
  content: "";
  position: absolute;
  left: 50%;
  top: 50%;
  width: calc(var(--pool-token-width, 44px) * 2.15);
  height: calc(var(--pool-token-width, 44px) * 2.15);
  transform: translate(-50%, -50%);
  border-radius: 999px;
}

/* Reveal when hovering wrapper OR the expanded hover zone OR focusing within */
.pool-item-wrap--active:hover .aux-arc,
.pool-item-wrap--active:focus-within .aux-arc,
.pool-item-wrap--active:hover.pool-item-wrap--sanity::before,
.pool-item-wrap--active:focus-within.pool-item-wrap--sanity::before {
}

.aux-arc {
  position: absolute;
  left: 50%;
  top: 50%;
  width: calc(var(--pool-token-width, 44px) * 2.0);
  height: calc(var(--pool-token-width, 44px) * 2.0);
  transform: translate(-50%, -50%) scale(0.92);
  transform-origin: center;
  opacity: 0;
  pointer-events: none;
  padding: 0;
  z-index: 10;
  transition: opacity 90ms ease, transform 160ms cubic-bezier(.2,.9,.2,1);
}

.pool-item-wrap--active:hover .aux-arc,
.pool-item-wrap--active:focus-within .aux-arc {
  opacity: 1;
  transform: translate(-50%, -50%) scale(1);
  pointer-events: none;
}

.aux-arc__svg {
  width: 100%;
  height: 100%;
  filter: drop-shadow(0 10px 18px rgba(0,0,0,0.30));
}

.aux-arc__base {
  fill: var(--ring);
  stroke: none;
  filter: drop-shadow(0 0 12px var(--ringSoft));
}

.aux-arc__hi {
  stroke: none;
  opacity: 0.95;
  mix-blend-mode: screen;
  pointer-events: none;
}

.aux-arc__hit {
  pointer-events: visiblePainted;
  cursor: pointer;
}

.aux-arc__labelText {
  fill: rgba(255, 255, 255, 0.95);
  paint-order: stroke;
  stroke: rgba(0, 0, 0, 0.25);
  stroke-width: 2px;
  stroke-linejoin: round;
  opacity: 0;
  transition: opacity 120ms ease;
  font-size: 11px;
  letter-spacing: 0.12em;
  font-weight: 900;
  pointer-events: none;
  text-transform: uppercase;
}

.pool-item-wrap--active:hover .aux-arc__labelText,
.pool-item-wrap--active:focus-within .aux-arc__labelText {
  opacity: 1;
  transition-delay: 40ms;
}

@media (prefers-reduced-motion: reduce) {
  .aux-arc,
  .aux-arc__labelText {
    transition: none;
  }
}

@media (hover: none) and (pointer: coarse) {
  .pool-item-wrap--active .aux-arc {
    opacity: 1;
    transform: translate(-50%, -50%) scale(1);
  }

  .pool-item-wrap--active .aux-arc__labelText {
    opacity: 1;
  }
}
</style>
