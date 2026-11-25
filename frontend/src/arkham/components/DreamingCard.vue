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

/* The dream counter borrows the resource coin's art (TokenPool.vue), which reads
 * as money on a card that has nothing to do with resources. The class is what
 * tints it; the token keeps its own 'Dream' tooltip. */
const DREAM_TOKEN_OVERRIDE = { Dream: { class: 'dream-token' } } as const

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
      <!-- Deliberately NOT `.card`: CardOverlay magnifies an `img.card` by its
           `src` and never looks at `data-image` (getImage, CardOverlay.vue), so
           the class would magnify the card BACK over a card whose face this
           viewer is entitled to see. Without it the overlay falls through to
           `data-image`, the way ConcealedCard's known-but-facedown art does. -->
      <img
        class="dreaming-card__face dreaming-card__face--back"
        :src="PLAYER_BACK"
        :data-image="hoverImage ?? undefined"
        :alt="$t('gameUi.dreamZone')"
      />
      <img
        v-if="faceImage"
        class="dreaming-card__face dreaming-card__face--front"
        :src="faceImage"
        :data-image="faceImage"
      />
    </div>
    <div v-if="hasPool" class="pool">
      <TokenPool :tokens="dreamingCard.tokens" :overrides="DREAM_TOKEN_OVERRIDE" />
    </div>
  </div>
</template>

<style scoped>
.dreaming-card {
  position: relative;
  width: var(--card-width);
  height: calc(var(--card-width) * 7 / 5);
  /* On the card rather than inside each transform, so both the in-zone flip and
     the leaving flip turn with the same depth. */
  perspective: 900px;
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
  user-select: none;
  /* Lifts the card off the zone's dark field and gives the back a moonlit edge. */
  box-shadow:
    0 2px 6px rgba(0, 0, 0, 0.55),
    0 0 10px rgba(130, 110, 230, 0.35);
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

/* Coin art, turned from gold to moonlight: a dream borrows the resource token's
   picture (TOKEN_CONFIG in TokenPool.vue) and would otherwise read as money.
   The drop-shadow PoolItem gives every token is restated because a `filter`
   declaration replaces the one it overrides rather than adding to it.
   Written flat rather than nested inside `.pool`: `:deep()` in a nested rule
   puts the scope attribute on the inner selector, which PoolItem's `img` -- in
   another component's scope -- does not carry, and the rule silently misses. */
.pool :deep(.dream-token img) {
  filter:
    hue-rotate(185deg)
    saturate(0.85)
    brightness(1.15)
    drop-shadow(1px 1px 2px rgb(0, 0, 0));
}

/* Leaving the zone. A dreaming card's exit is almost always
   `FlipDreamingCardFaceup` -- the record is removed and the card goes into play
   -- so it turns over on its way out instead of blinking away. The class comes
   from DreamZone's <TransitionGroup name="dream">, which keeps the zone mounted
   and its cell count steady for as long as this runs. */
.dreaming-card.dream-leave-active {
  animation: dream-leave 620ms ease forwards;
  pointer-events: none;
  z-index: var(--z-index-1);
}

.dreaming-card.dream-leave-active .dreaming-card__flip {
  animation: dream-leave-flip 620ms cubic-bezier(0.4, 0, 0.2, 1) forwards;
}

/* The counter does not turn with the card -- it is pinned over it -- so it has
   to be gone by the time the face comes round, or it reads as a token sitting
   on the asset. Out while the card is edge-on. */
.dreaming-card.dream-leave-active .pool {
  animation: dream-leave-pool 620ms ease forwards;
}

/* No front face to turn to (the registry has not caught up): keep the back
   facing the player rather than flipping to nothing, as the in-zone flip does. */
.dreaming-card.dream-leave-active .dreaming-card__face--back:only-child {
  backface-visibility: visible;
}

@keyframes dream-leave-flip {
  0% { transform: rotateY(0deg); }
  60%, 100% { transform: rotateY(180deg); }
}

@keyframes dream-leave {
  0%, 60% { opacity: 1; transform: scale(1); }
  100% { opacity: 0; transform: scale(1.06); }
}

@keyframes dream-leave-pool {
  0% { opacity: 1; }
  30%, 100% { opacity: 0; }
}

@media (prefers-reduced-motion: reduce) {
  .dreaming-card__flip {
    transition: none;
  }

  .dreaming-card.dream-leave-active,
  .dreaming-card.dream-leave-active .pool,
  .dreaming-card.dream-leave-active .dreaming-card__flip {
    animation: none;
  }
}
</style>
