<script lang="ts" setup>
import { computed } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Investigator } from '@/arkham/types/Investigator'
import { imgsrc } from '@/arkham/helpers'
import DreamingCardView from '@/arkham/components/DreamingCard.vue'

const props = defineProps<{
  game: Game
  investigator: Investigator
  playerId: string
  revealable: boolean
}>()

defineEmits<{ choose: [value: number] }>()

const DREAM_ICON = imgsrc('tokens/resource.png')

const dreamingCards = computed(() => props.investigator.dreamingCards ?? [])

const limit = computed(() => props.investigator.dreamingCardLimit ?? 0)

/* The cap is granted already resolved; it is never re-derived here. It can be
 * exceeded -- a cap-raising asset can leave play while the cards stay -- so the
 * row is as long as whichever is larger and nothing is ever clipped. */
const cells = computed(() => Math.max(limit.value, dreamingCards.value.length))

const emptyCells = computed(() => Math.max(0, cells.value - dreamingCards.value.length))

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
const show = computed(() => dreamingCards.value.length > 0)
</script>

<template>
  <div v-if="show" class="dream-zone">
    <!-- One root element, and nothing (not even a comment) outside it: this
         renders inside Player.vue's `.in-play` transition-group, whose children
         have to be single elements -- a fragment root is not animatable. -->
    <div class="dream-divider" />
    <h2 v-tooltip="roomTooltip ? $t('gameUi.dreamsRemaining', roomTooltip) : null">
      {{ $t('gameUi.dreamZone') }}
    </h2>
    <div class="dream-zone__cards">
      <DreamingCardView
        v-for="dreamingCard in dreamingCards"
        :key="dreamingCard.cardId"
        :game="game"
        :dreamingCard="dreamingCard"
        :playerId="playerId"
        :revealable="revealable"
        @choose="$emit('choose', $event)"
      />
      <div
        v-for="n in emptyCells"
        :key="`dream-empty-${n}`"
        class="slot dream-slot"
        v-tooltip="roomTooltip ? $t('gameUi.dreamsRemaining', roomTooltip) : null"
      >
        <img :src="DREAM_ICON" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.dream-divider {
  width: 2px;
  align-self: stretch;
  margin: 0 8px;
  background: rgba(255, 255, 255, 0.15);
  border-radius: 1px;
}

.dream-zone {
  display: flex;
  align-items: stretch;
  gap: 5px;

  h2 {
    text-align: center;
    text-transform: uppercase;
    font-size: 1.4vh;
    margin: 0;
    margin-top: -10px;
    margin-bottom: -10px;
    writing-mode: vertical-rl;
    orientation: mixed;
    color: rgba(255, 255, 255, 0.75);
  }
}

.dream-zone__cards {
  display: flex;
  gap: 5px;
  > * {
    flex-shrink: 0;
  }
}

/* Mirrors Player.vue's empty equipment `.slot`. Scoped styles do not reach into
   it from there, so the shape is restated here. */
.dream-slot {
  width: var(--card-width);
  background: rgba(0, 0, 0, 0.2);
  aspect-ratio: 5 / 7;
  height: calc(var(--card-width) * 7 / 5);
  border-radius: 6px;
  overflow: hidden;
  display: grid;
  place-items: center;
  border: 1px dashed rgba(255, 255, 255, 0.3);
  img {
    width: calc(var(--card-width) / 3);
    opacity: 0.5;
  }
}

/* Narrow portrait: the row is already scrolling sideways past the assets, so the
   empty cells are spent space. The label's tooltip still carries the count. */
@media (max-width: 800px) and (orientation: portrait) {
  .dream-slot {
    display: none;
  }
}
</style>
