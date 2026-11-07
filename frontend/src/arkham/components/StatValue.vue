<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Stat } from '@/arkham/statModel'
import { cardCodeOfEntity } from '@/arkham/attachments'
import { useCardNames } from '@/arkham/composables/useCardNames'

/* A stat's final number. When modifiers moved it away from the printed value it turns red (worse for the
 * investigators) or green (better), gets an arrow, and hovering it lists where the change came from. */
const props = withDefaults(defineProps<{
  stat: Stat | null
  game: Game
  /** Open the breakdown above (default) or below the number. */
  placement?: 'up' | 'down'
  /** Shown instead of the number, for a stat that is a dash or an X. */
  display?: string | null
}>(), { placement: 'up', display: null })

const { t } = useI18n()
const nameOf = useCardNames()

const rows = computed(() => (props.stat?.parts ?? []).map((part) => {
  const code = part.sourceId ? cardCodeOfEntity(props.game, part.sourceId) : null
  const label = code ? nameOf(code) : t('locationMap.stat.otherEffect')
  const text = part.mode === 'add' ? (part.delta > 0 ? `+${part.delta}` : `${part.delta}`) : `= ${part.delta + (props.stat?.printed ?? 0)}`
  return { label, text }
}))

const summary = computed(() => props.stat?.trend
  ? t('locationMap.stat.summary', { name: t(`locationMap.stat.${props.stat.key}`), printed: props.stat.printed, current: props.stat.current })
  : null)
</script>

<template>
  <span v-if="stat" class="stat" :class="[stat.trend ? { changed: true, worse: stat.bad, better: !stat.bad } : null, placement]"
    :tabindex="stat.trend ? 0 : undefined" :aria-label="summary ?? undefined">
    {{ display ?? stat.current }}<i v-if="stat.trend" class="arrow" aria-hidden="true">{{ stat.trend === 'up' ? '▲' : '▼' }}</i>
    <span v-if="stat.trend" class="why" role="tooltip">
      <b>{{ t(`locationMap.stat.${stat.key}`) }}</b>
      <span class="line"><em>{{ t('locationMap.stat.printed') }}</em><span>{{ stat.printed }}</span></span>
      <span v-for="(r, i) in rows" :key="i" class="line"><em>{{ r.label }}</em><span>{{ r.text }}</span></span>
      <span class="line total"><em>{{ t('locationMap.stat.now') }}</em><span>{{ stat.current }}</span></span>
    </span>
  </span>
</template>

<style scoped>
.stat { position: relative; display: inline-block; font-variant-numeric: tabular-nums; }
.stat.changed { cursor: help; font-weight: 700; }
.stat.worse { color: #ff8a7a; }
.stat.better { color: #8be0a0; }
.arrow { font-style: normal; font-size: 0.6em; margin-left: 1px; vertical-align: 0.15em; }
.why { display: none; position: absolute; left: 50%; translate: -50% 0; z-index: 80; min-width: 150px; padding: 6px 8px; gap: 2px; background: #0f1813f8; border: 1px solid #c6a873aa; border-radius: 6px; box-shadow: 0 6px 18px #000a; color: #ece6d0; font: 500 0.72rem/1.3 system-ui, sans-serif; white-space: nowrap; text-align: left; pointer-events: none; }
.stat.up .why { bottom: calc(100% + 6px); }
.stat.down .why { top: calc(100% + 6px); }
.stat:hover .why, .stat:focus-visible .why { display: grid; }
.why b { font: 0.8rem var(--ui-serif); color: #f1e6c1; }
.line { display: flex; justify-content: space-between; gap: 14px; }
.line em { font-style: normal; color: #cfc8ad; }
.line.total { border-top: 1px solid #ffffff22; margin-top: 2px; padding-top: 2px; font-weight: 700; }
</style>
