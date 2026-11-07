<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import type { MapLocationInfo } from '@/arkham/composables/useLocationInfo'
import { replaceText } from '@/arkham/cardTextFormat'

/* The name plate a location wears on the map: its name, whether it is revealed, and the numbers that
 * matter at a glance. Clicking it picks the location when a choice asks for one, otherwise opens its
 * info card; the ⓘ button only ever opens the card, so a target can be looked at before it is picked. */
const props = withDefaults(defineProps<{ info: MapLocationInfo; size?: 'full' | 'compact' | 'room'; showText?: boolean }>(), { size: 'full', showText: false })
const emit = defineEmits<{ (e: 'inspect', anchor: HTMLElement): void; (e: 'choose', index: number): void }>()

const { t } = useI18n()

// A room plate on a picture has space for the face-up side's text and flavour, shown without a click.
const textHtml = computed(() => props.size === 'room' && props.showText && props.info.face.text ? replaceText(props.info.face.text) : null)
const flavorHtml = computed(() => props.size === 'room' && props.showText && props.info.face.flavor ? replaceText(props.info.face.flavor) : null)

const summary = computed(() => {
  const i = props.info
  const parts = [i.face.name, i.revealed ? t('locationMap.marker.revealed') : t('locationMap.marker.unrevealed')]
  if (i.here) parts.push(t('locationMap.marker.youAreHere'))
  else if (i.adjacent) parts.push(t('locationMap.marker.adjacent'))
  if (i.shroud) parts.push(t('locationMap.marker.shroudValue', { n: i.shroud.x ? 'X' : i.shroud.current }))
  if (i.clues.left) parts.push(t('locationMap.marker.cluesValue', { n: i.clues.left }))
  if (i.onEnter) parts.push(t('locationMap.marker.onEnter'))
  if (i.targetable) parts.push(t('locationMap.marker.targetable'))
  return parts.join(', ')
})

// Hovering (or focusing) the Info button previews the card's face-up text, already in the player's language.
const preview = ref<{ left: number; top: number } | null>(null)
const previewTextHtml = computed(() => props.info.face.text ? replaceText(props.info.face.text) : null)
const previewFlavorHtml = computed(() => props.info.face.flavor ? replaceText(props.info.face.flavor) : null)
const PREVIEW_WIDTH = 300

function showPreview(event: Event) {
  const rect = (event.currentTarget as HTMLElement).getBoundingClientRect()
  const left = Math.max(8, Math.min(rect.left, window.innerWidth - PREVIEW_WIDTH - 8))
  const below = rect.bottom + 6
  // Open upwards when there is little room underneath.
  const top = below + 220 > window.innerHeight ? Math.max(8, rect.top - 6 - 220) : below
  preview.value = { left, top }
}
const hidePreview = () => { preview.value = null }

function anchorOf(event: Event): HTMLElement {
  return ((event.currentTarget as HTMLElement).closest('.plate') as HTMLElement | null) ?? (event.currentTarget as HTMLElement)
}

function onMain(event: Event) {
  if (props.info.actions.selectIndex !== -1) emit('choose', props.info.actions.selectIndex)
  else emit('inspect', anchorOf(event))
}
import GameIcon from '@/arkham/components/GameIcon.vue'
</script>

<template>
  <div class="plate" :class="[size, {
    revealed: info.revealed, here: info.here, adjacent: info.adjacent, targetable: info.targetable, exhausted: info.exhausted,
  }]">
    <div class="head">
      <button class="main" type="button" :aria-label="summary" :title="summary" @click.stop="onMain">
        <span class="name">{{ info.face.name }}</span>
        <span v-if="info.onEnter" class="warn" aria-hidden="true">⚠</span>
      </button>
      <button v-if="size !== 'compact'" class="info" type="button" :title="t('locationMap.marker.info')" :aria-label="t('locationMap.marker.details', { name: info.face.name })"
        data-no-card-overlay @mouseenter="showPreview" @mouseleave="hidePreview" @focus="showPreview" @blur="hidePreview"
        @click.stop="hidePreview(); emit('inspect', anchorOf($event))"><span aria-hidden="true">ⓘ</span></button>
    </div>
    <Teleport to="body">
      <div v-if="preview && (previewTextHtml || previewFlavorHtml || info.face.traits)" class="info-preview" role="tooltip"
        :style="{ left: preview.left + 'px', top: preview.top + 'px', width: PREVIEW_WIDTH + 'px' }">
        <strong class="pv-name">{{ info.face.name }}</strong>
        <em v-if="info.face.traits" class="pv-traits">{{ info.face.traits }}</em>
        <p v-if="previewTextHtml" class="pv-text" v-html="previewTextHtml"></p>
        <p v-if="previewFlavorHtml" class="pv-flavor" v-html="previewFlavorHtml"></p>
      </div>
    </Teleport>
    <div v-if="size !== 'compact' && info.revealed" class="stats">
      <template v-if="info.revealed">
        <span v-if="info.shroud" class="chip shroud" :class="{ modified: info.shroud.current !== info.shroud.printed }"
          :title="t('locationMap.marker.shroud') + (info.shroud.current !== info.shroud.printed ? ' · ' + t('locationMap.marker.printed', { n: info.shroud.printed }) : '')">
          🔍 {{ info.shroud.x ? 'X' : info.shroud.current }}</span>
        <span class="chip clue" :class="{ empty: !info.clues.left }" :title="t('locationMap.clues')"><GameIcon type="clue" /> {{ info.clues.left }}<template v-if="info.clues.printed !== null">/{{ info.clues.printed }}</template></span>
        <span v-if="info.doom" class="chip doom" :title="t('locationMap.doom')"><GameIcon type="doom" /> {{ info.doom }}</span>
      </template>
    </div>
    <!-- Card text comes from the bundled ArkhamDB card list, formatted the way the card overlay formats it. -->
    <div v-if="textHtml || flavorHtml" class="lore">
      <p v-if="textHtml" class="text" v-html="textHtml"></p>
      <p v-if="flavorHtml" class="flavor" v-html="flavorHtml"></p>
    </div>
    <button v-if="size !== 'compact' && info.actions.move" class="go" type="button" @click.stop="emit('choose', info.actions.move.index)">
      ↔ {{ t('locationMap.marker.moveHere') }}</button>
    <span v-else-if="size === 'compact' && info.clues.left" class="mini-clue"><GameIcon type="clue" />{{ info.clues.left }}</span>
  </div>
</template>

<style scoped>
.plate {
  --edge: #c6a873;
  display: inline-grid; justify-items: stretch; gap: 3px; min-width: 112px; max-width: max(180px, 22cqw);
  padding: 4px 6px 5px; border: 1px solid var(--edge); border-radius: 6px;
  background: #0f1712e6; box-shadow: 0 2px 10px #000c; backdrop-filter: blur(2px);
  color: #efe6cf; font: 600 11px/1.2 system-ui, sans-serif; text-align: left; pointer-events: auto;
}
.plate:not(.revealed) { --edge: #9a9d98; border-style: dashed; background: #5b5f5ae6; color: #ecebe4; }
.plate.here { --edge: #ffe9a0; box-shadow: 0 0 0 1px #ffe9a080, 0 0 14px #ffe9a066, 0 2px 10px #000c; }
.plate.adjacent { --edge: #9fc3a0; }
.plate.targetable { --edge: #f2d27a; animation: beckon 1.6s ease-in-out infinite; }
.plate.exhausted .name { color: #9a968c; }
@keyframes beckon { 50% { box-shadow: 0 0 0 2px #f2d27a, 0 0 18px #f2d27aaa; } }
@media (prefers-reduced-motion: reduce) { .plate.targetable { animation: none; box-shadow: 0 0 0 2px #f2d27a; } }
.head { display: flex; align-items: center; gap: 4px; }
button { font: inherit; color: inherit; background: none; border: 0; padding: 0; cursor: pointer; }
.main { display: flex; align-items: center; gap: 5px; flex: 1; min-width: 0; min-height: 22px; }
.main:focus-visible, .info:focus-visible, .go:focus-visible { outline: 2px solid #ffe9a0; outline-offset: 2px; border-radius: 3px; }
.name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font: 600 0.92rem/1.15 var(--ui-serif); font-variant: small-caps; letter-spacing: 0.02em; text-shadow: 0 1px 2px #000; }
.warn { color: #f0a868; }
.info { display: grid; place-items: center; width: 20px; height: 20px; padding: 0; border: 1px solid #d8c79f66; border-radius: 50%; color: #d8c79f; font-size: 10.5px; white-space: nowrap; }
.info:hover, .main:hover .name { color: #fff3cf; }
.info-preview { position: fixed; z-index: 10000; box-sizing: border-box; display: grid; gap: 5px; padding: 9px 11px; max-height: 260px; overflow-y: auto; pointer-events: none; background: #101913f7; border: 1px solid #c6a873; border-radius: 8px; box-shadow: 0 8px 24px #000b; color: #e9e3d0; font: 500 0.8rem/1.4 system-ui, sans-serif; }
.pv-name { font: 600 1rem/1.15 var(--ui-serif); font-variant: small-caps; color: #fff3cf; }
.pv-traits { color: #c9bf9f; }
.pv-text, .pv-flavor { margin: 0; }
.pv-flavor { font-style: italic; color: #c9bf9f; }
.stats { display: flex; flex-wrap: wrap; gap: 3px; }
.chip { padding: 1px 5px; border-radius: 9px; background: #ffffff12; font-variant-numeric: tabular-nums; white-space: nowrap; }
.chip.shroud { color: #d6d0bd; }
.chip.shroud.modified { color: #f5b26b; }
.chip.clue { color: #9fd3e6; }
.chip.clue.empty { color: #7d8c90; }
.chip.doom { color: #e0a595; }
.go { justify-self: start; padding: 2px 8px; border: 1px solid #9fc3a0; border-radius: 4px; background: #1d3324; color: #d7ecd2; font-size: 10.5px; }
.go:hover { background: #2a4a33; }
.plate.room { display: grid; width: 100%; max-width: none; min-width: 0; font-size: clamp(9px, 1.5cqh, 12px); }
.room .name { font-size: clamp(0.8rem, 2.1cqh, 1.05rem); }
.lore { display: grid; gap: 3px; max-height: 11em; overflow-y: auto; padding-top: 3px; border-top: 1px solid #c6a87333; font-weight: 500; line-height: 1.3; }
.lore p { margin: 0; }
.lore .flavor { font-style: italic; color: #c9bf9f; }
.plate.compact { min-width: 0; max-width: 100%; padding: 1px 3px; gap: 0; font-size: 8px; border-radius: 4px; box-shadow: none; backdrop-filter: none; }
.compact .name { font-size: 0.55rem; }
.compact .main { min-height: 0; gap: 2px; }
.mini-clue { color: #9fd3e6; font-size: 0.55rem; }
@media (max-width: 800px) {
  .plate.full .main { min-height: 44px; }
  .plate.full .info { height: 44px; width: 44px; }
  .plate.full .go { min-height: 44px; }
}
</style>
