<script lang="ts" setup>
import { computed, ref } from 'vue'
import PoolItem from '@/arkham/components/PoolItem.vue'
import { type Token, type Tokens } from '@/arkham/types/Token'

export type TokenPoolItem = {
  key: string
  type: string
  amount?: number
  tooltip?: string
  class?: unknown
  force?: boolean
}

export type TokenPoolOverride = {
  type?: string
  tooltip?: string
  class?: unknown
  force?: boolean
}

const TOKEN_CONFIG: Partial<Record<Token, { type: string; tooltip?: string }>> = {
  AlarmLevel: { type: 'doom', tooltip: 'Alarm Level' },
  Clue: { type: 'clue' },
  Damage: { type: 'health' },
  Doom: { type: 'doom' },
  Horror: { type: 'horror' },
  Resource: { type: 'resource' },
  Pillar: { type: 'resource' },
  Kindling: { type: 'resource' },
  Leyline: { type: 'resource', tooltip: 'Leyline' },
  Shard: { type: 'resource', tooltip: 'Shard' },
  ScoutingReport: { type: 'resource', tooltip: 'Scouting Report' },
  Scrap: { type: 'resource', tooltip: 'Scrap' },
  Depletion: { type: 'resource', tooltip: 'Scouting Report' },
  Antiquity: { type: 'resource', tooltip: 'Antiquity' },
  Civilian: { type: 'resource', tooltip: 'Civilian' },
  Study: { type: 'resource', tooltip: 'Civilian' },
  Target: { type: 'resource', tooltip: 'Target' },
  Time: { type: 'resource', tooltip: 'Time' },
  Newspaper: { type: 'resource', tooltip: 'Newspaper' },
  Shipment: { type: 'resource', tooltip: 'Shipment' },
  Seed: { type: 'resource', tooltip: 'Seed' },
  TimeCapsule: { type: 'resource', tooltip: 'Time Capsule' },
  Seal: { type: 'resource', tooltip: 'Seal' },
  Depth: { type: 'resource' },
  LostSoul: { type: 'resource' },
  Overgrowth: { type: 'resource' },
  Bounty: { type: 'resource' },
  Evidence: { type: 'resource', tooltip: 'Evidence' },
  Warning: { type: 'resource', tooltip: 'Warning' },
  Memory: { type: 'resource', tooltip: 'Memory' },
  Brilliance: { type: 'resource', tooltip: 'Brilliance' },
  Charge: { type: 'resource' },
}

const props = withDefaults(defineProps<{
  tokens?: Tokens
  order?: readonly Token[]
  overrides?: Partial<Record<Token, TokenPoolOverride>>
  extraItems?: readonly TokenPoolItem[]
}>(), {
  tokens: () => ({}),
  overrides: () => ({}),
  extraItems: () => [],
})

const emit = defineEmits<{ choose: [key: string] }>()

const tokenKeys = computed<Token[]>(() => {
  if (props.order) return [...props.order]

  const keys = Object.keys(props.tokens) as Token[]
  for (const key of Object.keys(props.overrides) as Token[]) {
    if (!keys.includes(key)) keys.push(key)
  }
  return keys
})

const tokenItems = computed<TokenPoolItem[]>(() => tokenKeys.value.flatMap((token) => {
  const amount = props.tokens[token] ?? 0
  const config = TOKEN_CONFIG[token] ?? { type: 'resource', tooltip: token }
  const override = props.overrides[token] ?? {}
  const force = override.force ?? false
  if (!force && amount <= 0) return []

  return [{
    key: token,
    type: override.type ?? config.type,
    tooltip: override.tooltip ?? config.tooltip,
    class: override.class,
    amount,
    force,
  }]
}))

const items = computed(() => [
  ...props.extraItems.filter((item) => item.force || (item.amount ?? 0) > 0),
  ...tokenItems.value,
])

// When there are more tokens than fit comfortably, clump them into an
// overlapping stack and spread them out (scaled up) on hover — mirroring the
// sealed-chaos-token popover.
const CLUMP_THRESHOLD = 3
const clumped = computed(() => items.value.length > CLUMP_THRESHOLD)
const expanded = ref(false)
</script>

<template>
  <div
    class="token-pool"
    :class="{ 'token-pool--clumped': clumped, 'token-pool--expanded': expanded }"
    @mouseenter="expanded = true"
    @mouseleave="expanded = false"
  >
    <PoolItem
      v-for="item in items"
      :key="item.key"
      :type="item.type"
      :amount="item.amount"
      :tooltip="item.tooltip"
      :class="item.class"
      @choose="emit('choose', item.key)"
    />
  </div>
</template>

<style scoped>
/* Not clumped: behave as before — pass tokens straight into the parent .pool
   flex layout. */
.token-pool {
  display: contents;
}

.token-pool--clumped {
  display: inline-flex;
  position: relative;
  align-items: center;
  border-radius: 999px;
  transition: padding 0.16s ease, background 0.16s ease;
}

.token-pool--clumped :deep(.poolItem) {
  transition: margin 0.16s ease, transform 0.16s ease;
}

/* Collapsed: overlap into a compact stack. */
.token-pool--clumped :deep(.poolItem + .poolItem) {
  margin-left: calc(var(--card-token-width) * -0.5);
}

/* Expanded: float above the card, spread out and scale up slightly. */
.token-pool--clumped.token-pool--expanded {
  z-index: var(--z-index-30000, 30000);
  background: rgba(0, 0, 0, 0.72);
  padding: 3px 7px;
}

.token-pool--clumped.token-pool--expanded :deep(.poolItem + .poolItem) {
  margin-left: 3px;
}

.token-pool--clumped.token-pool--expanded :deep(.poolItem) {
  transform: scale(1.3);
}
</style>
