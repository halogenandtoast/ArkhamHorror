<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import { attachmentsOf, type HostKind } from '@/arkham/attachments'
import type { EffectLine } from '@/arkham/statModel'
import { cardImage } from '@/arkham/cardImages'
import { useCardNames } from '@/arkham/composables/useCardNames'

/* The cards attached to a location, enemy or investigator, drawn as small round chips on the host rather
 * than as cards. A chip shows the card's art and any counters; hovering it says in one line what it does to
 * its host (the final numbers are on the host itself) and the full card appears through the usual card
 * preview, because the chip carries the card's image. Past `max` chips the rest fold into a "+N" button. */
const props = withDefaults(defineProps<{
  game: Game
  host: HostKind
  hostId: string
  max?: number
  placement?: 'up' | 'down'
}>(), { max: 3, placement: 'up' })

const { t } = useI18n()
const nameOf = useCardNames()
const expanded = ref(false)

const all = computed(() => attachmentsOf(props.game, props.host, props.hostId))
const shown = computed(() => expanded.value ? all.value : all.value.slice(0, props.max))
const hidden = computed(() => Math.max(0, all.value.length - props.max))

const COUNTER_ICON = { doom: '☠', clue: '◇', resource: '$', damage: '♥', horror: '◆' } as const

function effectText(line: EffectLine): string {
  if (line.kind === 'flag') return t(`locationMap.flag.${line.flag}`)
  const name = t(`locationMap.stat.${line.stat}`)
  if (line.mode === 'add') return `${name} ${line.delta > 0 ? '+' : ''}${line.delta}`
  return t(line.mode === 'base' ? 'locationMap.stat.baseIs' : 'locationMap.stat.setTo', { name, value: line.delta })
}
</script>

<template>
  <span v-if="all.length" class="chips" :class="placement" :aria-label="t('locationMap.attach.label')" @click.stop>
    <span v-for="a in shown" :key="a.id" class="chip" :class="[a.tone, { exhausted: a.exhausted }]" tabindex="0"
      :data-image="cardImage(a.cardCode)" :data-card-code="a.cardCode">
      <span class="art" :style="{ backgroundImage: `url(${cardImage(a.cardCode)})` }"></span>
      <b v-for="c in a.counters" :key="c.token" class="counter" :class="c.token">{{ COUNTER_ICON[c.token] }}{{ c.count }}</b>
      <span class="tip" role="tooltip">
        <b>{{ nameOf(a.cardCode) }}</b>
        <span v-for="(line, i) in a.effects" :key="i" class="effect">{{ effectText(line) }}</span>
        <span v-if="!a.effects.length" class="effect quiet">{{ t('locationMap.attach.readCard') }}</span>
        <span v-if="a.counters.length" class="effect quiet">{{ a.counters.map((c) => `${COUNTER_ICON[c.token]} ${c.count}`).join('  ') }}</span>
      </span>
    </span>
    <button v-if="hidden && !expanded" type="button" class="more" :title="t('locationMap.attach.more', { n: hidden })" @click="expanded = true">+{{ hidden }}</button>
    <button v-else-if="hidden" type="button" class="more" :title="t('locationMap.attach.less')" @click="expanded = false">−</button>
  </span>
</template>

<style scoped>
.chips { position: relative; display: inline-flex; flex-wrap: wrap; justify-content: center; gap: 3px; max-width: 130px; }
.chip { position: relative; width: 26px; height: 26px; border-radius: 50%; border: 2px solid var(--tone, #c6a873); background: #111815; box-shadow: 0 1px 4px #000c; cursor: help; }
.chip.harm { --tone: #d9695b; }
.chip.aid { --tone: #d9b45b; }
.chip.exhausted { opacity: 0.6; }
.chip:focus-visible { outline: 2px solid #ffe9a0; }
.art { position: absolute; inset: 0; border-radius: 50%; background-size: 240% auto; background-position: 50% 22%; background-repeat: no-repeat; }
.counter { position: absolute; right: -6px; bottom: -5px; min-width: 14px; padding: 0 3px; border-radius: 7px; background: #0b100d; border: 1px solid #ffffff44; color: #ffe9a0; font: 700 9px/13px system-ui, sans-serif; text-align: center; }
.counter.doom { color: #ff9b87; }
.counter + .counter { right: auto; left: -6px; }
.more { width: 26px; height: 26px; padding: 0; border-radius: 50%; border: 1px solid #c6a873aa; background: #101a15e6; color: #f1e6c1; font: 700 10px/1 system-ui, sans-serif; cursor: pointer; }
.more:hover { background: #26352bf0; }
.tip { display: none; position: absolute; left: 50%; translate: -50% 0; z-index: 90; min-width: 140px; max-width: 220px; padding: 6px 8px; gap: 2px; background: #0f1813f8; border: 1px solid #c6a873aa; border-radius: 6px; box-shadow: 0 6px 18px #000a; color: #ece6d0; font: 500 0.72rem/1.3 system-ui, sans-serif; text-align: left; white-space: normal; pointer-events: none; }
.chips.up .tip { bottom: calc(100% + 6px); }
.chips.down .tip { top: calc(100% + 6px); }
.chip:hover .tip, .chip:focus-visible .tip { display: grid; }
.tip b { font: 0.82rem var(--ui-serif); color: #f1e6c1; }
.effect { color: #e9e2c6; }
.effect.quiet { color: #a9b0a0; }
</style>
