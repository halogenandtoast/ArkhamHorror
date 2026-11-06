<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'
import { TokenType } from '@/arkham/types/Token'
import { portraitImage } from '@/arkham/cardImages'
import { imgsrc } from '@/arkham/helpers'
import { useCardNames } from '@/arkham/composables/useCardNames'

/* One seat of the team overlay. Purely presentational: it shows the investigator's state and
 * reports clicks; choosing a seat is done by PlayerTabs, which owns the selected tab. */
const props = defineProps<{
  game: Game
  investigator: Investigator
  selected: boolean
  active: boolean
  lead: boolean
  hasActions: boolean
  waiting: boolean
  eliminated: boolean
  solo: boolean
  canSwitch: boolean
  switchable: boolean
}>()
const emit = defineEmits<{
  (e: 'select'): void
  (e: 'perspective'): void
}>()

const { t } = useI18n()
const nameOf = useCardNames()
const inv = computed(() => props.investigator)
const cls = computed(() => inv.value.class)
const portrait = computed(() => portraitImage(inv.value.cardCode))
const crown = computed(() => imgsrc('tokens/lead-investigator.png'))
const tok = (type: keyof typeof inv.value.tokens) => inv.value.tokens[type] ?? 0
const meter = (max: number, taken: number) => ({ max, left: Math.max(0, max - taken), pct: max > 0 ? (100 * Math.max(0, max - taken)) / max : 0 })
const skills = computed(() => [
  ['willpower', inv.value.willpower], ['intellect', inv.value.intellect], ['combat', inv.value.combat], ['agility', inv.value.agility],
] as const)
const hp = computed(() => meter(inv.value.health, tok(TokenType.Damage)))
const sanity = computed(() => meter(inv.value.sanity, tok(TokenType.Horror)))
</script>

<template>
  <div class="seat" :class="[`class-${cls}`, { selected, active, eliminated }]" role="button" tabindex="0"
    :aria-pressed="selected" @click="emit('select')" @keydown.enter.prevent="emit('select')">
    <span class="face">
      <img :src="portrait" :alt="nameOf(inv.cardCode)" />
      <img v-if="lead" class="crown" :src="crown" :alt="t('locationMap.lead')" />
      <i v-if="hasActions" class="pulse" aria-hidden="true"></i>
    </span>
    <span class="body">
      <span class="name"><b v-if="active" class="play" aria-hidden="true">▶</b>{{ nameOf(inv.cardCode) }}</span>
      <span class="meter hp"><span class="track"><i :style="{ width: hp.pct + '%' }"></i></span><em>♥ {{ hp.left }}/{{ hp.max }}</em></span>
      <span class="meter sanity"><span class="track"><i :style="{ width: sanity.pct + '%' }"></i></span><em>◆ {{ sanity.left }}/{{ sanity.max }}</em></span>
    </span>
    <span class="side">
      <span class="count res" :title="t('locationMap.resources')">$ {{ tok(TokenType.Resource) }}</span>
      <span class="count clue" :title="t('locationMap.clues')">◇ {{ tok(TokenType.Clue) }}</span>
      <span v-if="waiting" class="waiting" :title="t('locationMap.waiting')">⟳</span>
      <span v-if="!waiting" class="count hand" :title="t('locationMap.hand', { count: inv.hand?.length ?? 0 })">🂠 {{ inv.hand?.length ?? '–' }}</span>
    </span>
    <div class="detail" role="tooltip">
      <b>{{ nameOf(inv.cardCode) }}</b>
      <span>♥ {{ hp.left }}/{{ hp.max }} · ◆ {{ sanity.left }}/{{ sanity.max }}</span>
      <span class="skills"><i v-for="[k, v] in skills" :key="k">{{ k.slice(0, 3) }} {{ v }}</i></span>
      <span>{{ tok(TokenType.Resource) }} {{ t('locationMap.resources').toLowerCase() }} · {{ tok(TokenType.Clue) }} {{ t('locationMap.clues').toLowerCase() }}</span>
      <span>{{ t('locationMap.hand', { count: inv.hand?.length ?? 0 }) }} · {{ t('locationMap.actionsLeft', { count: inv.remainingActions }) }}</span>
    </div>
    <button v-if="solo && switchable" class="eye" :class="{ glow: canSwitch }" :title="t('locationMap.switchTo')"
      @click.stop="emit('perspective')">👁</button>
  </div>
</template>

<style scoped>
.seat { flex: none; box-sizing: border-box; --c: #8a9a86; position: relative; display: flex; align-items: center; gap: 8px; width: 188px; padding: 4px 6px; background: #0f1813b8; border: 1px solid color-mix(in srgb, var(--c) 70%, transparent); border-radius: 8px; backdrop-filter: blur(4px); color: #e5e2d4; cursor: pointer; transition: transform 0.12s, box-shadow 0.12s; }
.seat:hover { background: #0f1813f2; }
.seat.class-Guardian { --c: #4a8fd0; } .seat.class-Seeker { --c: #d9a441; } .seat.class-Rogue { --c: #4fae6a; }
.seat.class-Mystic { --c: #9a63d1; } .seat.class-Survivor { --c: #d1584f; }
.seat.selected { outline: 2px solid #ffe9a0; outline-offset: -2px; }
.seat.active .face { box-shadow: 0 0 0 2px #ffe9a0, 0 0 10px #ffe9a0aa; }
.seat.eliminated { opacity: 0.4; filter: grayscale(1); }
.face { position: relative; flex: none; width: 36px; height: 36px; border-radius: 50%; border: 3px solid var(--c); }
.face > img:first-child { width: 100%; height: 100%; border-radius: 50%; object-fit: cover; object-position: 50% 20%; }
.crown { position: absolute; left: -8px; top: -8px; width: 18px; height: 18px; }
.pulse { position: absolute; right: -3px; top: -3px; width: 10px; height: 10px; border-radius: 50%; background: #ffe9a0; animation: blink 1.4s ease-in-out infinite alternate; }
.body { display: grid; gap: 3px; min-width: 0; flex: 1; font: 600 11px/1.15 system-ui, sans-serif; }
.name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font: 0.78rem Georgia, serif; }
.play { color: #ffe9a0; margin-right: 4px; font-size: 0.7rem; }
.meter { display: flex; align-items: center; gap: 5px; }
.meter .track { flex: 1; height: 4px; background: #5b5a54; border-radius: 3px; overflow: hidden; }
.meter .track i { display: block; height: 100%; background: currentColor; }
.meter em { font-style: normal; flex: none; white-space: nowrap; text-align: right; color: #fff4df; }
.meter .track { min-width: 16px; }
.meter.hp { color: #ff9b87; } .meter.sanity { color: #9fceee; }
.side { display: grid; gap: 1px; justify-items: end; font: 600 10px/1.2 system-ui, sans-serif; }
.count.res { color: #f0d9a8; } .count.clue { color: #9fd3e6; } .count.hand { color: #c9d0c3; }
.waiting { animation: spin 3s linear infinite; color: #ffe9a0; }
.eye { position: absolute; right: 4px; top: 4px; background: #0008; border: 0; border-radius: 4px; color: #aaa; font-size: 0.7rem; cursor: pointer; }
.eye.glow { color: #ffe9a0; }
.detail { display: none; position: absolute; left: 100%; top: 0; z-index: 5; margin-left: 8px; min-width: 190px; padding: 8px 10px; gap: 3px; background: #0f1813f5; border: 1px solid #c6a873aa; border-radius: 8px; box-shadow: 0 6px 20px #000a; font: 500 0.72rem/1.3 system-ui, sans-serif; }
.seat:hover .detail { display: grid; }
.detail b { font: 0.9rem Georgia, serif; }
.detail .skills { display: flex; gap: 8px; text-transform: uppercase; white-space: nowrap; color: #d6d0b6; }
.detail .skills i { font-style: normal; }
@keyframes blink { to { opacity: 0.35; } }
@keyframes spin { to { transform: rotate(360deg); } }
@media (prefers-reduced-motion: reduce) { .pulse, .waiting { animation: none; } }
@media (max-width: 800px) {
  .seat:not(.selected) { width: auto; padding: 3px; }
  .seat:not(.selected) .body, .seat:not(.selected) .side, .seat:not(.selected) .eye { display: none; }
}
</style>
