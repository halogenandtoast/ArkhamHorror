<script lang="ts" setup>
import { computed } from 'vue'
import * as CardT from '@/arkham/types/Card'
import * as ArkhamGame from '@/arkham/types/Game'
import type { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { Message, MessageType } from '@/arkham/types/Message'
import type { DreamingCard } from '@/arkham/types/DreamingCard'
import TokenPool from '@/arkham/components/TokenPool.vue'

const props = defineProps<{
  game: Game
  dreamingCard: DreamingCard
  playerId: string
  /* Whether this viewer may peek at what the card is. Not a secrecy boundary --
   * every client is sent every card in the game -- just whose zone it is. */
  revealable: boolean
}>()

const emit = defineEmits<{ choose: [value: number] }>()

const PLAYER_BACK = imgsrc('backs/back_player.jpg')

const cardId = computed(() => props.dreamingCard.cardId)

const card = computed(() => props.dreamingCard.card ?? props.game.cards[cardId.value])

/* The printed face, whichever side the registry happens to have it facedown on:
 * a dreaming card is stored facedown, and that is a placement, not its art. */
const faceImage = computed(() => {
  const c = card.value
  if (!c) return null
  return imgsrc(CardT.cardImage({ ...CardT.toCardContents(c), facedown: false }))
})

const flipped = computed(() => props.dreamingCard.flipped)

/* The owner (or a solo/show-all viewer) gets the real art under the cursor via
 * CardOverlay, which hovers anything carrying `data-image`. */
const hoverImage = computed(() => (props.revealable ? faceImage.value : null))

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function isCardAction(c: Message): boolean {
  if (c.tag !== MessageType.TARGET_LABEL) return false
  if (c.target.contents !== cardId.value) return false
  return c.target.tag === 'DreamingCardTarget' || c.target.tag === 'CardIdTarget'
}

const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => cardAction.value !== -1)

const hasPool = computed(() =>
  Object.values(props.dreamingCard.tokens).some((amount) => (amount ?? 0) > 0)
)

function clicked() {
  if (cardAction.value !== -1) emit('choose', cardAction.value)
}
</script>

<template>
  <div
    class="dreaming-card"
    :class="{ 'dreaming-card--can-interact': canInteract }"
    :style="{ viewTransitionName: `dream-${cardId}` }"
    @click="clicked"
  >
    <div class="dreaming-card__flip" :class="{ 'dreaming-card__flip--flipped': flipped }">
      <img
        class="card dreaming-card__face dreaming-card__face--back"
        :src="PLAYER_BACK"
        :data-image="hoverImage ?? undefined"
        :alt="$t('gameUi.dreamZone')"
      />
      <img
        v-if="faceImage"
        class="card dreaming-card__face dreaming-card__face--front"
        :src="faceImage"
        :data-image="faceImage"
      />
    </div>
    <div v-if="hasPool" class="pool">
      <TokenPool :tokens="dreamingCard.tokens" />
    </div>
  </div>
</template>

<style scoped>
.dreaming-card {
  position: relative;
  width: var(--card-width);
  height: calc(var(--card-width) * 7 / 5);
}

.dreaming-card--can-interact {
  cursor: pointer;
}

.dreaming-card__flip {
  position: relative;
  width: 100%;
  height: 100%;
  transform-style: preserve-3d;
  transition: transform 0.35s ease;
}

.dreaming-card__flip--flipped {
  transform: rotateY(180deg);
}

.dreaming-card__face {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  object-fit: cover;
  border-radius: 5px;
  backface-visibility: hidden;
}

.dreaming-card__face--front {
  transform: rotateY(180deg);
}

/* A card that has not loaded into the registry yet has no front face at all, so
   the back must stay visible even once it flips. */
.dreaming-card__flip--flipped .dreaming-card__face--back:only-child {
  transform: rotateY(180deg);
  backface-visibility: visible;
}

.dreaming-card--can-interact .dreaming-card__face {
  border: 2px solid var(--select);
}

.pool {
  position: absolute;
  top: 50%;
  left: 0;
  display: flex;
  align-items: flex-end;
  z-index: var(--z-index-1);
  pointer-events: none;
  :deep(img) {
    width: var(--card-token-width);
    height: auto;
  }
}

@media (prefers-reduced-motion: reduce) {
  .dreaming-card__flip {
    transition: none;
  }
}
</style>
