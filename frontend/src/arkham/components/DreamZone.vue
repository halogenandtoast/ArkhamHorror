<script lang="ts" setup>
import { computed, reactive } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'
import DreamingCardView from '@/arkham/components/DreamingCard.vue'

const props = defineProps<{
  game: Game
  investigator: Investigator
  playerId: string
  revealable: boolean
}>()

defineEmits<{ choose: [value: number] }>()

const dreamingCards = computed(() => props.investigator.dreamingCards ?? [])

/* Cards still running their leaving flip. They are gone from game state but
 * still on screen, and both the cell count and the zone's own existence have to
 * hold still until the animation is over -- otherwise a placeholder slides in
 * under the card mid-flip, or the last card out unmounts the zone and takes its
 * own animation with it. */
const leavingEls = reactive(new Set<Element>())
const leaving = computed(() => leavingEls.size)

/* Rather longer than the 620ms animation. `after-leave` is what normally clears
 * an element; a leave the parent interrupts never fires it, and a count that
 * never came back down would hold an empty zone on screen for good. */
const LEAVE_GIVE_UP_MS = 1500

function onBeforeLeave(el: Element) {
  leavingEls.add(el)
  window.setTimeout(() => leavingEls.delete(el), LEAVE_GIVE_UP_MS)
}

function onAfterLeave(el: Element) {
  leavingEls.delete(el)
}

const limit = computed(() => props.investigator.dreamingCardLimit ?? 0)

/* The cap is granted already resolved; it is never re-derived here. It can be
 * exceeded -- a cap-raising asset can leave play while the cards stay -- so the
 * row is as long as whichever is larger and nothing is ever clipped. */
const cells = computed(() => Math.max(limit.value, dreamingCards.value.length))

const emptyCells = computed(() =>
  Math.max(0, cells.value - dreamingCards.value.length - leaving.value)
)

/* Room left in the zone. At the cap this is 0 and every cell is filled, which is
 * the whole of the at-the-cap feedback: the cap is a cost the engine charges,
 * not a prohibition, so nothing is badged. */
const remaining = computed(() => Math.max(0, limit.value - dreamingCards.value.length))

/* No granted cap means no claim about the room left -- without this, a payload
 * that omits the limit would read as "no room left" on a zone holding one card. */
const limitKnown = computed(() => props.investigator.dreamingCardLimit != null)

const roomTooltip = computed(() => limitKnown.value ? { count: remaining.value } : null)

/* Gated on holding a dreaming card, NOT on the granted limit.
 * `getDreamingCardLimit` (Helpers/Dreaming.hs) answers `3 + AdditionalDreamingCard`
 * for every investigator in every campaign, so a limit of 3 says nothing about
 * whether this seat can dream at all -- gating on it would put three dead cells
 * in Night of the Zealot. The zone appears with the first dreamed card and the
 * placeholders then fill out to the cap. */
const show = computed(() => dreamingCards.value.length > 0 || leaving.value > 0)
</script>

<template>
  <div v-if="show" class="dream-zone">
    <!-- One root element, and nothing (not even a comment) outside it: this
         renders inside Player.vue's `.in-play` transition-group, whose children
         have to be single elements -- a fragment root is not animatable.

         The panel itself is what names the zone now that no label does: the
         cards sit on their own field of night rather than in line with the
         assets beside them. -->
    <!-- No `tag`: the cards stay direct children of the zone's flex row. The
         leaving flip itself is DreamingCard's (`.dream-leave-active`). -->
    <TransitionGroup name="dream" @before-leave="onBeforeLeave" @after-leave="onAfterLeave">
      <DreamingCardView
        v-for="dreamingCard in dreamingCards"
        :key="dreamingCard.cardId"
        :game="game"
        :dreamingCard="dreamingCard"
        :playerId="playerId"
        :revealable="revealable"
        @choose="$emit('choose', $event)"
      />
    </TransitionGroup>
    <div
      v-for="n in emptyCells"
      :key="`dream-empty-${n}`"
      class="dream-slot"
      v-tooltip="roomTooltip ? $t('gameUi.dreamsRemaining', roomTooltip) : null"
    >
      <svg class="dream-slot__glyph" viewBox="0 0 24 24" aria-hidden="true">
        <path d="M20.6 15.1A8.6 8.6 0 0 1 9.5 4a8.6 8.6 0 1 0 11.1 11.1Z" />
        <path class="dream-slot__spark" d="M17.2 3.2l.7 1.8 1.8.7-1.8.7-.7 1.8-.7-1.8-1.8-.7 1.8-.7z" />
        <path class="dream-slot__spark" d="M21 9.4l.45 1.15L22.6 11l-1.15.45L21 12.6l-.45-1.15L19.4 11l1.15-.45z" />
      </svg>
    </div>
  </div>
</template>

<style scoped>
/* A field of night the dreaming cards lie on, no taller than the cards on it.
   `align-self` is load-bearing: `.in-play` stretches its children, so without it
   the panel is pulled to the height of the tallest card in the row -- an asset
   wearing a "needs slot(s)" badge made it 25px taller than its own contents.
   The negative vertical margin then cancels the padding and border, so the
   panel bleeds into `.in-play`'s own 10px padding rather than adding to the
   height of the play strip. */
.dream-zone {
  /* One pad on every edge, so the field reads the same width above and below a
     card as it does beside one. The negative margin cancels it and the border,
     which is what keeps the panel's outer height equal to a card's. */
  --dream-pad: 6px;

  display: flex;
  align-self: center;
  align-items: center;
  gap: 5px;
  padding: var(--dream-pad);
  margin: calc(-1px - var(--dream-pad)) 3px;
  border-radius: 10px;
  border: 1px solid rgba(154, 134, 255, 0.28);
  background:
    radial-gradient(90% 120% at 18% 0%, rgba(126, 103, 224, 0.26), rgba(126, 103, 224, 0) 70%),
    linear-gradient(180deg, rgba(31, 24, 61, 0.75), rgba(12, 10, 28, 0.75));
  box-shadow:
    inset 0 1px 0 rgba(196, 182, 255, 0.14),
    0 0 16px rgba(104, 82, 194, 0.22);

  > * {
    flex-shrink: 0;
  }
}

.dream-slot {
  width: var(--card-width);
  aspect-ratio: 5 / 7;
  height: calc(var(--card-width) * 7 / 5);
  border-radius: 6px;
  display: grid;
  place-items: center;
  border: 1px dashed rgba(172, 155, 255, 0.32);
  background: rgba(8, 6, 22, 0.35);
}

.dream-slot__glyph {
  width: calc(var(--card-width) / 2.4);
  height: auto;
  fill: rgba(190, 176, 255, 0.4);
}

.dream-slot__spark {
  fill: rgba(216, 207, 255, 0.5);
}

/* Narrow portrait: the row is already scrolling sideways past the assets, so the
   empty cells are spent space. The slot's tooltip goes with them; the panel is
   what still says the cards are dreaming. */
@media (max-width: 800px) and (orientation: portrait) {
  .dream-slot {
    display: none;
  }

  .dream-zone {
    --dream-pad: 4px;

    margin-inline: 2px;
  }
}
</style>
