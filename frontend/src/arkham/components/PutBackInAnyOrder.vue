<script lang="ts" setup>
import { computed, inject, onUnmounted, watch } from 'vue';
import { useI18n } from 'vue-i18n';
import { useDbCardStore } from '@/stores/dbCards';
import { imgsrc } from '@/arkham/helpers';
import { cardArt } from '@/arkham/cardImages';
import { cardImage, toCardContents } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
import Card from '@/arkham/components/Card.vue';
import {
  putBackInAnyOrderPicks,
  putBackArrangement,
  putBackSubmitting,
  putBackSignature,
  putBackSignatureOf,
  resetPutBack,
  type PutBackPick,
} from '@/arkham/putBackInAnyOrder';

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
const { t } = useI18n()
const store = useDbCardStore()

// Provided by the game view, which owns the websocket and the question state.
const chooseOrdered = inject<(choices: number[]) => void>('chooseOrdered')

// Putting cards back in any order. Every pick is buffered, so cards can be
// placed and taken back, and Done sends the whole arrangement as one ordered
// answer. The column fills from the bottom up, which is also the order the
// choices are answered in, since each one puts its card on top of the deck.
const putBackPicks = computed(() => putBackInAnyOrderPicks(props.game, props.playerId))

// Whatever is left of the arrangement that the current question still offers,
// bottom card first. Anything else (a stale prompt, a reload) is dropped.
const putBackPlaced = computed<PutBackPick[]>(() => {
  const picks = putBackPicks.value
  if (!picks) return []
  return putBackArrangement.value.flatMap((id) => picks.find((pick) => pick.id === id) ?? [])
})

const putBackUnplaced = computed<PutBackPick[]>(() => {
  const placed = new Set(putBackPlaced.value.map((pick) => pick.id))
  return (putBackPicks.value ?? []).filter((pick) => !placed.has(pick.id))
})

// A placed card keeps its spot in the tray as a ghost, so placing one never
// resizes the modal out from under the cursor.
const putBackIsPlaced = (pick: PutBackPick) =>
  putBackArrangement.value.includes(pick.id)

// Left to right is top of the deck to bottom: the open slots, then the
// arrangement, whose first card sits at the far right.
const putBackSlots = computed<(PutBackPick | null)[] | null>(() => {
  if (!putBackPicks.value) return null
  return [...Array<null>(putBackUnplaced.value.length).fill(null), ...[...putBackPlaced.value].reverse()]
})

// The open slot next to the cards already placed: the next one goes there.
const putBackNextSlot = computed(() => putBackUnplaced.value.length - 1)

const putBackReady = computed(() => !!putBackPicks.value && putBackUnplaced.value.length === 0)

const placePutBack = (pick: PutBackPick) => {
  if (putBackSubmitting.value || putBackIsPlaced(pick)) return
  putBackArrangement.value = [...putBackArrangement.value, pick.id]
}

const unplacePutBack = (pick: PutBackPick) => {
  if (putBackSubmitting.value) return
  putBackArrangement.value = putBackArrangement.value.filter((id) => id !== pick.id)
}

/* One answer for the whole placement: the engine resolves every choice in this
 * order in a single pass, so it is one action and one undo step. Answering card
 * by card left a step per card, and undoing into the middle of a placement
 * stranded the rest. */
const submitPutBack = () => {
  const picks = putBackPicks.value
  if (putBackSubmitting.value || !putBackReady.value || !picks) return
  if (!chooseOrdered) return
  putBackSubmitting.value = true
  chooseOrdered(putBackArrangement.value.flatMap((id) =>
    picks.find((pick) => pick.id === id)?.index ?? []
  ))
}

// A prompt for a different set of cards starts from nothing, whatever is left
// over. `onUnmounted` below covers the usual case; this covers a buffer that
// somehow outlived it (a reload mid-arrangement, say).
watch(putBackPicks, (picks) => {
  if (!picks) return resetPutBack()

  const signature = putBackSignatureOf(picks)
  if (putBackSignature.value !== signature) {
    resetPutBack()
    putBackSignature.value = signature
  }
}, { immediate: true })

/* Always: the answer is one shot, so once the panel is gone there is nothing
 * left to protect. Keeping the buffer because a submission was in flight left
 * the arrangement and the disabled Done in place for the next prompt -- and an
 * undo brings back the same cards, so the signature check below would not
 * clear it either. */
onUnmounted(resetPutBack)

const putBackName = (pick: PutBackPick) => {
  const { cardCode } = toCardContents(pick.card)
  return store.getDbCard(cardArt(cardCode))?.name ?? ''
}

</script>

<template>
<div class="modal">
          <div class="modal-contents put-back" :class="{ 'put-back--submitting': putBackSubmitting }">
            <div class="group put-back__tray">
              <div class="put-back__cards">
                <div
                  v-for="pick in putBackPicks"
                  :key="pick.id"
                  class="put-back__card"
                  :class="{ 'put-back__card--ghost': putBackIsPlaced(pick) }"
                >
                  <Card
                    :card="pick.card"
                    :game="game"
                    :playerId="playerId"
                    :allowInteractions="!putBackIsPlaced(pick) && !putBackSubmitting"
                    :allowAbilityButtons="false"
                    @choose="placePutBack(pick)"
                  />
                </div>
              </div>
            </div>
            <div class="group put-back__deck">
              <div class="put-back__slots">
                <div
                  v-for="(slot, i) in putBackSlots"
                  :key="slot ? slot.id : `open-${i}`"
                  class="put-back__slot"
                >
                  <button
                    v-if="slot"
                    type="button"
                    class="put-back__placed"
                    :aria-disabled="putBackSubmitting"
                    :aria-label="t('putBackInAnyOrder.takeBack', { name: putBackName(slot) })"
                    @click="unplacePutBack(slot)"
                  >
                    <img class="put-back__thumb" :src="imgsrc(cardImage(slot.card))" alt="" />
                    <span class="put-back__take-back" aria-hidden="true">
                      <svg viewBox="0 0 24 24">
                        <path d="M9 14 4 9l5-5" />
                        <path d="M4 9h11a5 5 0 0 1 0 10h-3" />
                      </svg>
                    </span>
                  </button>
                  <span
                    v-else
                    class="put-back__open"
                    :class="{ 'put-back__open--next': i === putBackNextSlot }"
                  ></span>
                </div>
              </div>
              <div class="put-back__axis">
                <span class="put-back__edge put-back__edge--top">
                  <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M19 12H5" /><path d="m12 19-7-7 7-7" /></svg>
                  {{ t('putBackInAnyOrder.top') }}
                </span>
                <span class="put-back__edge">
                  {{ t('putBackInAnyOrder.bottom') }}
                  <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M5 12h14" /><path d="m12 5 7 7-7 7" /></svg>
                </span>
              </div>
              <button
                type="button"
                class="put-back__submit"
                :aria-disabled="!putBackReady || putBackSubmitting"
                @click="submitPutBack"
              >
                {{ t('label.done') }}
              </button>
            </div>
          </div>
</div>
</template>

<style scoped>
/* Own component, own stylesheet: the panel is deliberately not part of
   Question.vue's scoped styles, whose blanket `button` rule paints every button
   with the purple `--button-2` fill, 10px of padding and an ArkhamIcons glyph
   via `::before`. These few rules are the modal chrome it still wants. */
.modal-contents {
  display: flex;
  padding: 10px;
}

/* The same panel the modal's other card groups use. */
.group {
  display: flex;
  align-items: stretch;
  flex-direction: column;
  gap: 10px;
  box-sizing: border-box;
  padding: 12px;
  border: 1px solid rgba(214, 205, 174, 0.18);
  border-radius: 10px;
  background: rgba(20, 16, 24, 0.74);
  box-shadow:
    inset 0 0 0 1px rgba(0, 0, 0, 0.35),
    0 6px 18px rgba(0, 0, 0, 0.24);
}

/* Putting cards back in any order. The stack under the cards is the whole
   explanation: top of the deck on the left, and the next card placed drops into
   the open slot nearest the cards already placed. */
.put-back {
  /* One card size for both rows, so a card is the same size wherever it is. */
  --card-width: min(120px, 22vw);
  flex-direction: column;
  align-items: stretch;
  gap: 12px;
  /* Cards over a row of small ones: sized to them, not stretched to the width
     every other card group wants. */
  width: auto;
  max-width: 100%;
}

/* Both panels wear the modal's own `.group` chrome, so this reads as part of
   the same modal as every other card group. */
.put-back__tray {
  width: auto;
}

.put-back__deck {
  width: auto;
  gap: 8px;
}

.put-back__cards {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}

/* Placed cards stay in the tray, greyed out, so the tray keeps its size and the
   card keeps its place while the arrangement is built. */
.put-back__card--ghost {
  opacity: 0.28;
  filter: grayscale(0.7);
  pointer-events: none;
}

.put-back__slots {
  display: flex;
  gap: 8px;
}

.put-back__slot {
  display: flex;
}

.put-back__placed,
.put-back__open {
  box-sizing: border-box;
  width: var(--card-width);
  aspect-ratio: var(--card-aspect);
  margin: 2px;
  padding: 0;
  border: 0;
  border-radius: 6px;
}

/* A filled slot is the card and nothing else: no frame, no padding, no button
   fill (this file gives every bare `button` the purple `--button-2`). */
.put-back .put-back__placed {
  position: relative;
  overflow: hidden;
  background: none;
  background-image: none;
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  cursor: pointer;
}

.put-back__thumb {
  display: block;
  width: 100%;
  height: 100%;
  object-fit: cover;
}

/* The return icon is the affordance, and it only appears over the card the
   pointer is on. */
.put-back__take-back {
  position: absolute;
  inset: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(10, 12, 16, 0.62);
  opacity: 0;
  transition: opacity 0.12s ease;
}

.put-back__take-back svg {
  width: 45%;
  height: 45%;
  fill: none;
  stroke: #fff;
  stroke-width: 2;
  stroke-linecap: round;
  stroke-linejoin: round;
}

.put-back .put-back__placed:hover:not([aria-disabled='true']) .put-back__take-back,
.put-back .put-back__placed:focus-visible .put-back__take-back {
  opacity: 1;
}

.put-back .put-back__placed:focus-visible {
  outline: 2px solid var(--background-light);
  outline-offset: 2px;
}

.put-back__placed[aria-disabled='true'] {
  cursor: default;
  opacity: 0.6;
}

/* Touch has no hover, so there the icon is always on, at a weight that still
   lets the card read through it. */
@media (hover: none) {
  .put-back__take-back {
    inset: auto 0 0 auto;
    width: 22px;
    height: 22px;
    border-radius: 7px 0 0 0;
    opacity: 1;
  }

  .put-back__take-back svg {
    width: 68%;
    height: 68%;
  }
}

/* An empty slot is the one thing that still needs an outline: it has no card to
   show, and the row has to read as four positions. */
.put-back__open {
  border: 1px dashed var(--box-border);
  background-color: var(--background-dark);
}

.put-back__open--next {
  border: 2px solid var(--highlight);
  background-color: rgba(45, 212, 191, 0.12);
}

.put-back__axis {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 16px;
}

.put-back__edge {
  display: flex;
  align-items: center;
  gap: 5px;
  color: #8b95a8;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.12em;
  text-transform: uppercase;
}

.put-back__edge--top {
  color: var(--seeker);
}

.put-back__edge svg {
  width: 13px;
  height: 13px;
  fill: none;
  stroke: currentColor;
  stroke-width: 2.4;
  stroke-linecap: round;
  stroke-linejoin: round;
}

/* Purple when it is ready: that is the app's forward-progress button, and this
   is the click that commits the arrangement. Neutral while it is not available,
   so it does not invite a click that does nothing. `aria-disabled`, not
   `disabled` -- the handler already refuses, and this keeps it focusable. */
.put-back .put-back__submit {
  width: 100%;
  padding: 10px 18px;
  border: 0;
  border-radius: 8px;
  background-color: var(--button-2);
  background-image: none;
  color: #eee;
  font-family: inherit;
  font-size: 0.85rem;
  font-weight: bold;
  letter-spacing: 0.08em;
  text-align: center;
  text-transform: uppercase;
  cursor: pointer;
  transition: background-color 0.2s ease-in;
}

.put-back .put-back__submit:hover:not([aria-disabled='true']) {
  background-color: var(--button-2-highlight);
}

.put-back .put-back__submit[aria-disabled='true'] {
  background-color: var(--background-dark);
  color: var(--background-light);
  cursor: default;
  opacity: 0.55;
}

.put-back--submitting .put-back__tray {
  pointer-events: none;
  opacity: 0.55;
}

@media (max-width: 700px) {
  .put-back {
    --card-width: min(96px, 26vw);
  }
}

@media (prefers-reduced-motion: reduce) {
  .put-back__take-back {
    transition: none;
  }
}
</style>
