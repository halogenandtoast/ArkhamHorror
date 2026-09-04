<script lang="ts" setup>
import { computed, nextTick, onBeforeUnmount, ref, watch } from 'vue'
import { Dropdown } from 'floating-vue'
import { type Card as ArkhamCard, type CardContents, cardImage, toCardContents } from '@/arkham/types/Card'
import { imgsrc } from '@/arkham/helpers'
import type { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import CardView from '@/arkham/components/Card.vue'
import { useDebug } from '@/arkham/debug'

const props = withDefaults(defineProps<{
  cards: (ArkhamCard | CardContents)[]
  label?: string
  placement?: 'top' | 'bottom' | 'left' | 'right'
  game?: Game
  playerId?: string
  isDiscards?: boolean
  highlighted?: boolean
  showLabel?: boolean
  shown?: boolean
  fullWidth?: boolean
  vertical?: boolean
  droppable?: boolean
  draggableCards?: boolean
}>(), {
  label: 'Cards underneath',
  placement: 'bottom',
})

const emit = defineEmits<{
  choose: [value: number]
  'update:shown': [value: boolean]
  cardsDrop: [event: DragEvent]
  cardDragStart: [event: DragEvent, index: number]
}>()

const debug = useDebug()
const internalShown = ref(false)
const restoreAfterDrag = ref(false)
const shown = computed({
  get: () => props.shown ?? internalShown.value,
  set: (value: boolean) => {
    if (props.shown === undefined) internalShown.value = value
    emit('update:shown', value)
  },
})

// floating-vue positions the popper once and pins that transform, so a popover
// that loses a card keeps its old left edge and drifts away from the trigger.
// Ask it to recompute whenever the contents change underneath it.
const dropdown = ref<{ onResize?: () => void } | null>(null)

async function reposition() {
  await nextTick()
  dropdown.value?.onResize?.()
  // onResize is a no-op until floating-vue's own isShown catches up, which it
  // does asynchronously, so take a second pass once it has settled.
  window.setTimeout(() => dropdown.value?.onResize?.(), 60)
}

const count = computed(() => props.cards.length)

watch(count, () => reposition())
const tooltip = computed(() => `${props.label} (${count.value}) — click to view`)
const choices = computed(() => props.game && props.playerId ? ArkhamGame.choices(props.game, props.playerId) : [])
const interactive = computed(() => props.game !== undefined && props.playerId !== undefined)

function isCardInChoices(card: ArkhamCard | CardContents): boolean {
  const cardId = toCardContents(card).id
  return choices.value.some(choice => {
    if (choice.tag === 'TargetLabel') return choice.target.tag === 'CardIdTarget' && cardId === choice.target.contents
    if (choice.tag === 'AbilityLabel') {
      const sourceId = choice.ability.source.sourceTag === 'OtherSource' ? choice.ability.source.contents : undefined
      if (!sourceId) return false
      if (cardId === sourceId) return true
      const asset = props.game?.assets[sourceId]
      return asset?.cardId === cardId
    }
    return false
  })
}

const hasCardChoice = computed(() => props.cards.some(isCardInChoices))
const isHighlighted = computed(() => props.highlighted || hasCardChoice.value)

function finishDrag() {
  window.removeEventListener('dragend', finishDrag)
  window.removeEventListener('drop', finishDrag)
  if (!restoreAfterDrag.value) return
  restoreAfterDrag.value = false
  // The card that just left is still in `cards` at this point; reopening now
  // would size and place the popover against content it is about to lose,
  // leaving a gap where the card was. Wait for the list to settle first.
  nextTick(() => {
    if (count.value === 0) return
    shown.value = true
    reposition()
  })
}

// Dragging a card out of the popover means dropping it somewhere the popover is
// currently covering, so get it out of the way and put it back afterwards.
function hidePopoverWhileDragging() {
  const dragsOut = props.draggableCards || (debug.active && props.isDiscards)
  if (!dragsOut || !shown.value) return
  restoreAfterDrag.value = true
  shown.value = false
  window.addEventListener('dragend', finishDrag, { once: true })
  window.addEventListener('drop', finishDrag, { once: true })
}

function onCardDragStart(event: DragEvent, index: number) {
  if (!props.draggableCards) return
  emit('cardDragStart', event, index)
  hidePopoverWhileDragging()
}

// The drag sources here declare effectAllowed 'copyMove'; answering with a
// dropEffect outside that set makes the browser refuse the drop outright.
function onDragOver(event: DragEvent) {
  if (!props.droppable) return
  event.preventDefault()
  if (event.dataTransfer) event.dataTransfer.dropEffect = 'move'
}

// dragenter/dragleave fire again for every child element, so count depth rather
// than clearing the highlight the first time the pointer crosses one.
const dragDepth = ref(0)
const draggedOver = computed(() => dragDepth.value > 0)

function onDragEnter(event: DragEvent) {
  if (!props.droppable) return
  event.preventDefault()
  dragDepth.value++
}

function onDragLeave() {
  if (props.droppable) dragDepth.value = Math.max(0, dragDepth.value - 1)
}

function onDrop(event: DragEvent) {
  if (!props.droppable) return
  event.preventDefault()
  dragDepth.value = 0
  emit('cardsDrop', event)
}

onBeforeUnmount(() => finishDrag())
</script>

<template>
  <Dropdown
    ref="dropdown"
    :placement="placement"
    :distance="8"
    v-model:shown="shown"
    :disabled="count === 0"
    :triggers="['click']"
    :auto-hide="true"
    theme="cards-under-popover"
  >
    <button
      type="button"
      class="cards-under-indicator"
      :class="{ 'cards-under-indicator--highlighted': isHighlighted, 'cards-under-indicator--with-label': showLabel, 'cards-under-indicator--full-width': fullWidth, 'cards-under-indicator--vertical': vertical, 'cards-under-indicator--dragged-over': draggedOver }"
      :aria-label="tooltip"
      v-tooltip="tooltip"
      @dragover="onDragOver"
      @dragenter="onDragEnter"
      @dragleave="onDragLeave"
      @drop="onDrop"
    >
      <span
        class="cards-under-indicator__icon"
        :class="{ 'cards-under-indicator__icon--custom': !!$slots.icon }"
        aria-hidden="true"
      >
        <slot name="icon">
          <span class="cards-under-indicator__card cards-under-indicator__card--back" />
          <span class="cards-under-indicator__card cards-under-indicator__card--front" />
        </slot>
      </span>
      <span v-if="showLabel" class="cards-under-indicator__label">{{ label }}</span>
      <span class="cards-under-indicator__count">{{ count }}</span>
    </button>

    <template #popper>
      <div
        class="cards-under-popover"
        :class="{ 'cards-under-popover--dragged-over': draggedOver }"
        @dragover="onDragOver"
        @dragenter="onDragEnter"
        @dragleave="onDragLeave"
        @drop="onDrop"
      >
        <div class="cards-under-popover__header">{{ label }} ({{ count }})</div>
        <div class="cards-under-popover__cards" @dragstart="hidePopoverWhileDragging">
          <div
            v-for="(card, i) in cards"
            :key="i"
            class="cards-under-popover__card-wrap"
            :class="{ discard: isDiscards && !isCardInChoices(card), 'cards-under-popover__card-wrap--draggable': draggableCards }"
            :draggable="draggableCards || undefined"
            @dragstart="onCardDragStart($event, i)"
          >
            <CardView
              v-if="interactive && game && playerId"
              :game="game"
              :playerId="playerId"
              :card="card"
              @choose="emit('choose', $event)"
            />
            <img
              v-else
              :src="imgsrc(cardImage(card))"
              class="card cards-under-popover__card"
            />
          </div>
        </div>
      </div>
    </template>
  </Dropdown>
</template>

<style scoped>
.cards-under-indicator {
  display: flex !important;
  align-items: center;
  justify-content: center;
  gap: 7px;
  height: 22px;
  padding: 0 7px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.16);
  background: rgba(0, 0, 0, 0.46);
  color: #fff;
  line-height: 1;
  cursor: pointer;
  backdrop-filter: blur(4px);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.35);
  transition: background 0.15s ease, border-color 0.15s ease, transform 0.15s ease;
}

.cards-under-indicator:hover {
  background: rgba(0, 0, 0, 0.68);
  border-color: rgba(255, 255, 255, 0.32);
  transform: translateY(-1px);
}

.cards-under-indicator--highlighted {
  border-color: color-mix(in srgb, var(--select) 65%, black);
  background: color-mix(in srgb, var(--select) 55%, black);
  color: #fff;
  box-shadow: 0 0 8px color-mix(in srgb, var(--select) 45%, transparent);
}

.cards-under-indicator--highlighted:hover {
  background: color-mix(in srgb, var(--select) 65%, black);
  border-color: color-mix(in srgb, var(--select) 75%, black);
}

/* Pending drop target — the receiver of the drag, so cyan, matching
   `ability-target` rather than the magenta reserved for awaited choices. */
.cards-under-indicator.cards-under-indicator--dragged-over {
  border-color: var(--highlight);
  background: color-mix(in srgb, var(--highlight) 40%, black);
  box-shadow: 0 0 0 2px color-mix(in srgb, var(--highlight) 70%, transparent),
    0 0 6px 1px color-mix(in srgb, var(--highlight) 45%, transparent);
}

.cards-under-indicator--with-label {
  width: 100%;
  justify-content: center;
}

.cards-under-indicator--full-width {
  width: var(--card-width) !important;
  min-width: var(--card-width);
  max-width: var(--card-width);
  justify-content: center;
}

.cards-under-indicator__icon {
  width: 20px;
  height: 15px;
  flex: 0 0 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

/* A supplied icon fills the slot the stacked-cards glyph would have used, but
   keeps its upright reading direction even when the pill is turned on its side. */
.cards-under-indicator__icon--custom :deep(svg) {
  width: 14px;
  height: 14px;
}

.cards-under-indicator__card {
  flex: 0 0 auto;
  width: 10px;
  height: 13px;
  border-radius: 2px;
  border: 1px solid rgba(255, 255, 255, 0.8);
  background: rgba(255, 255, 255, 0.18);
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.45);
}

.cards-under-indicator__card--back {
  opacity: 0.55;
  transform: rotate(-6deg) translateY(1px);
}

.cards-under-indicator__card--front {
  margin-left: -4px;
  background: rgba(255, 255, 255, 0.28);
  transform: rotate(6deg) translateY(-1px);
}

.cards-under-indicator__label {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.02em;
  color: rgba(255, 255, 255, 0.86);
}

.cards-under-indicator__count {
  min-width: 1.35em;
  height: 1.35em;
  display: inline-grid;
  place-items: center;
  border-radius: 999px;
  font-size: 0.72rem;
  font-weight: 800;
  line-height: 1;
  background: rgba(255, 255, 255, 0.14);
  border: 1px solid rgba(255, 255, 255, 0.22);
  font-variant-numeric: tabular-nums;
}

@media (max-width: 800px) {
  .cards-under-indicator {
    gap: 0;
    padding: 0 5px;
  }

  .cards-under-indicator__icon {
    display: none;
  }
}

/* Vertical variant: the same pill turned on its side, for pinning down the
   edge of a play area rather than sitting in a row of controls. */
.cards-under-indicator--vertical {
  flex-direction: column;
  width: 22px;
  height: auto;
  min-height: 60px;
  padding: 7px 0;
}

.cards-under-indicator--vertical:hover {
  transform: translateX(-1px);
}

.cards-under-indicator--vertical .cards-under-indicator__icon {
  transform: rotate(90deg);
}

/* ...but a supplied icon reads as an icon, not as a turned-on-its-side glyph,
   so it stays upright. */
.cards-under-indicator--vertical .cards-under-indicator__icon--custom {
  transform: none;
}

.cards-under-indicator--vertical .cards-under-indicator__label {
  writing-mode: vertical-rl;
  max-height: 14em;
}

@media (max-width: 800px) {
  .cards-under-indicator--vertical {
    padding: 5px 0;
  }
}

.cards-under-popover {
  min-width: 0;
  max-width: max(50vw, 300px);
  padding: 10px;
}

.cards-under-popover--dragged-over {
  outline: 2px dashed color-mix(in srgb, var(--highlight) 70%, transparent);
  outline-offset: -4px;
  border-radius: 8px;
}

.cards-under-popover__header {
  margin: 0 0 8px;
  color: rgba(255, 255, 255, 0.82);
  font-size: 0.85rem;
  font-weight: 700;
  letter-spacing: 0.02em;
  text-transform: uppercase;
}

.cards-under-popover__cards {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  align-items: flex-start;
  gap: 6px;
  max-height: 50vh;
  overflow: auto;
}

.cards-under-popover__card-wrap {
  position: relative;
  flex: 0 0 auto;
}

.cards-under-popover__card-wrap--draggable {
  cursor: grab;
}

.cards-under-popover__card-wrap.discard {
  filter: grayscale(0.85);
}

.cards-under-popover__card-wrap :deep(.card),
.cards-under-popover__card {
  width: calc(var(--card-width, 100px) * 1.1);
  border-radius: 6px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.6);
}

.cards-under-popover__card-wrap :deep(.card-container) {
  margin: 0;
}
</style>

<style>
.v-popper__popper.v-popper--theme-cards-under-popover {
  z-index: calc(var(--z-card-hover-overlay) - 1);
}

.v-popper--theme-cards-under-popover .v-popper__inner {
  background: rgba(15, 15, 20, 0.92);
  backdrop-filter: blur(8px);
  border: 1px solid rgba(255, 255, 255, 0.12);
  border-radius: 10px;
  color: #fff;
  box-shadow: 0 8px 30px rgba(0, 0, 0, 0.5);
}

.v-popper--theme-cards-under-popover .v-popper__arrow-outer {
  border-color: rgba(255, 255, 255, 0.12);
}

.v-popper--theme-cards-under-popover .v-popper__arrow-inner {
  border-color: rgba(15, 15, 20, 0.92);
}
</style>
