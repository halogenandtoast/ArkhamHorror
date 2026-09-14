<script lang="ts" setup>
import { computed, ref } from 'vue'
import { chaosTokenImage, compareTokenFaces, tokenFaceDifference, type TokenFace } from '@/arkham/types/ChaosToken'
import { type ChaosBagChange } from '@/arkham/types/Campaign'
import { campaignStepIcon, campaignStepName } from '@/arkham/types/CampaignStep'
import type { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n'

const props = defineProps<{ game: Game, history: ChaosBagChange[] }>()

const { t } = useI18n()

type TokenGroup = { face: TokenFace, count: number }

// Identical faces collapse into one swatch with a ×N badge.
const group = (faces: TokenFace[]): TokenGroup[] => {
  const counts = new Map<TokenFace, number>()
  for (const face of faces) counts.set(face, (counts.get(face) ?? 0) + 1)
  return [...counts.entries()]
    .map(([face, count]) => ({ face, count }))
    .sort((a, b) => compareTokenFaces(a.face, b.face))
}

type BagToken = { face: TokenFace, state: 'kept' | 'added' | 'removed' }

/**
 * The bag as it ends up, plus the tokens that left it, in one sorted list:
 * green for what arrived, red for what was taken out. Faces repeat, so the
 * added flag is spent per instance: N added skulls mark N of the skulls present.
 */
const unify = (after: TokenFace[], added: TokenFace[], removed: TokenFace[]): BagToken[] => {
  const pending = new Map<TokenFace, number>()
  for (const face of added) pending.set(face, (pending.get(face) ?? 0) + 1)
  const kept: BagToken[] = after.map((face) => {
    const left = pending.get(face) ?? 0
    if (left === 0) return { face, state: 'kept' }
    pending.set(face, left - 1)
    return { face, state: 'added' }
  })
  const gone: BagToken[] = removed.map((face) => ({ face, state: 'removed' }))
  return [...kept, ...gone].sort((a, b) => compareTokenFaces(a.face, b.face))
}

type Entry = {
  name: string
  icon: string | null
  added: TokenGroup[]
  removed: TokenGroup[]
  bag: BagToken[]
  count: number
}

const entries = computed<Entry[]>(() =>
  props.history
    .map((change) => {
      const added = tokenFaceDifference(change.after, change.before)
      const removed = tokenFaceDifference(change.before, change.after)
      return {
        name: campaignStepName(props.game, change.step),
        icon: campaignStepIcon(change.step),
        added: group(added),
        removed: group(removed),
        bag: unify(change.after, added, removed),
        count: change.after.length,
      }
    })
    .filter((entry) => entry.added.length > 0 || entry.removed.length > 0)
)

const expanded = ref<Set<number>>(new Set())

const toggle = (index: number) => {
  const next = new Set(expanded.value)
  if (next.has(index)) next.delete(index)
  else next.add(index)
  expanded.value = next
}
</script>

<template>
  <div class="changes">
    <div class="change" v-for="(entry, idx) in entries" :key="idx">
      <button
        type="button"
        class="change-header"
        :aria-expanded="expanded.has(idx)"
        @click="toggle(idx)"
      >
        <img v-if="entry.icon" :src="entry.icon" class="scenario-icon" />
        <span class="step-name">{{ entry.name }}</span>
        <span class="deltas">
          <span class="delta added" v-for="g in entry.added" :key="`+${g.face}`">
            <span class="sign">+</span>
            <img class="token" :src="chaosTokenImage(g.face)" :title="g.face" />
            <span class="multiplier" v-if="g.count > 1">×{{ g.count }}</span>
          </span>
          <span class="delta removed" v-for="g in entry.removed" :key="`-${g.face}`">
            <span class="sign">−</span>
            <img class="token" :src="chaosTokenImage(g.face)" :title="g.face" />
            <span class="multiplier" v-if="g.count > 1">×{{ g.count }}</span>
          </span>
        </span>
        <svg class="chevron" :class="{ collapsed: !expanded.has(idx) }" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="6 9 12 15 18 9"/></svg>
      </button>
      <div class="bag" v-if="expanded.has(idx)">
        <h4>{{ t('campaignLog.chaosBag') }} <span class="count-pill">{{ entry.count }}</span></h4>
        <div class="tokens">
          <span
            v-for="(token, i) in entry.bag"
            :key="`${token.face}${i}`"
            class="token-slot"
            :class="token.state"
          >
            <img class="token" :src="chaosTokenImage(token.face)" :title="token.face" />
          </span>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.changes {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.change {
  background: rgba(255,255,255,0.03);
  border: 1px solid rgba(255,255,255,0.06);
  border-radius: 6px;
  overflow: hidden;
}

.change-header {
  display: flex;
  align-items: center;
  gap: 10px;
  width: 100%;
  padding: 8px 10px;
  background: none;
  border: none;
  color: inherit;
  font: inherit;
  text-align: left;
  cursor: pointer;

  &:hover .step-name { color: rgba(255,255,255,0.85); }
}

/* Opt out of the global button press effect: these rows expand in place. */
button.change-header:active:not(:disabled) {
  transform: none;
}

.scenario-icon {
  width: 22px;
  height: 22px;
  object-fit: contain;
  flex-shrink: 0;
  filter: brightness(0) invert(1) opacity(0.75);
}

.step-name {
  flex: 1;
  font-family: teutonic, sans-serif;
  font-size: 0.95em;
  color: rgba(255,255,255,0.6);
  letter-spacing: 0.04em;
  overflow-wrap: break-word;
}

.deltas {
  display: flex;
  flex-wrap: wrap;
  justify-content: flex-end;
  gap: 5px;
}

.delta {
  display: inline-flex;
  align-items: center;
  gap: 3px;
  padding: 2px 6px 2px 4px;
  border-radius: 4px;
  font-size: 0.78rem;
  font-weight: 700;
  white-space: nowrap;
}

.delta.added {
  background: rgba(74,196,86,0.15);
  color: #6dd97a;
  border: 1px solid rgba(74,196,86,0.25);
}

.delta.removed {
  background: rgba(180,30,30,0.2);
  color: #e07878;
  border: 1px solid rgba(180,30,30,0.35);
}

.delta .token {
  width: 20px;
  height: 20px;
}

.delta.removed .token { opacity: 0.75; }

.chevron {
  width: 1.1em;
  height: 1.1em;
  color: rgba(255,255,255,0.3);
  flex-shrink: 0;
  transition: transform 0.2s ease;

  &.collapsed { transform: rotate(-90deg); }
}

.bag {
  padding: 0 10px 10px;

  h4 {
    display: flex;
    align-items: center;
    gap: 6px;
    font-family: teutonic, sans-serif;
    font-size: 0.85em;
    font-weight: normal;
    color: rgba(255,255,255,0.5);
    text-transform: uppercase;
    letter-spacing: 0.06em;
    margin: 0 0 6px;
  }
}

.tokens {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
}

.token-slot {
  position: relative;
  display: inline-flex;
  border-radius: 50%;

  /* The overlay tints the art itself, so a marked token reads at a glance
     without changing the bag's layout. */
  &::after {
    content: '';
    position: absolute;
    inset: 0;
    border-radius: 50%;
    pointer-events: none;
  }

  &.removed::after {
    background: rgba(180,30,30,0.55);
    box-shadow: inset 0 0 0 2px #e07878;
  }

  &.added::after {
    background: rgba(74,196,86,0.45);
    box-shadow: inset 0 0 0 2px #6dd97a;
  }
}

.tokens .token {
  width: 30px;
  height: 30px;
  border-radius: 50%;
  border: 1px solid rgba(255,255,255,0.2);
  box-shadow: 0 2px 4px rgba(0,0,0,0.5);
}
</style>
