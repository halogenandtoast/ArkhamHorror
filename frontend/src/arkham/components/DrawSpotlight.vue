<script lang="ts" setup>
import { computed } from 'vue'
import { type Card, cardId } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { CARD_FLIGHT_TRANSITION_CLASS, cardFlightTransitionName } from '@/arkham/cardFlight'
import CardView from '@/arkham/components/Card.vue'

/* The enlarged look at cards you just drew: the encounter reveal's treatment,
 * applied to your own deck. It holds `uiLock` and waits for you, then hands the
 * cards off to your hand with a view transition.
 *
 * Its own component rather than more markup in Game.vue: that file's
 * `.revelation` block styles every bare `button` purple with 10px padding and a
 * hover colour -- the same blanket-element trap Question.vue has -- so anything
 * added inside it inherits rules no class rule reliably beats. For the same
 * reason the flip keyframes are redeclared here: Vue mangles `@keyframes` names
 * per scope, so Game.vue's `flip-front` is not reachable from a child.
 *
 * Cards render through Card.vue and never as a hand-rolled `<img class="card">`.
 * CardOverlay.vue derives the hover magnifier entirely from the hovered
 * element's `src` plus `data-customizations` / `data-chained` / `data-card-code`
 * / `data-image-id`, so a bare img silently loses taboo art and the
 * customization sheet (#5734).
 */
const props = defineProps<{
  game: Game
  playerId: string
  title: string
  cards: Card[]
}>()

const emit = defineEmits<{ dismiss: [] }>()

/* Above this a draw stops being a reveal and becomes a list: the cards overlap
 * into a fan and the title carries the count. Deliberately not a setting -- the
 * complaint it answers ("the card that made me draw 6 is annoying") is about a
 * parade of modals, and one fan is not a parade. */
const FAN_THRESHOLD = 3

const isFan = computed(() => props.cards.length > FAN_THRESHOLD)

/* Half of the flight to the hand: this overlay names the card, and Game.vue
 * puts the same name on the hand card at the moment this overlay is torn down.
 * See cardFlight.ts for why they must never hold it simultaneously. */
const cardStyle = (card: Card, i: number) => ({
  '--i': i,
  viewTransitionName: cardFlightTransitionName(card),
  viewTransitionClass: CARD_FLIGHT_TRANSITION_CLASS,
})

const backFor = (card: Card) =>
  imgsrc(card.tag === 'PlayerCard' ? 'backs/back_player.jpg' : 'backs/back_encounter.jpg')
</script>

<template>
  <div
    class="spotlight"
    :class="{ 'spotlight--fan': isFan }"
    role="dialog"
    aria-modal="true"
    @click="emit('dismiss')"
  >
    <div class="spotlight__inner">
      <h2 class="spotlight__title">{{ title }}</h2>

      <div class="spotlight__cards" :style="{ '--count': cards.length }">
        <div
          v-for="(card, i) in cards"
          :key="cardId(card)"
          class="spotlight__card"
          :style="cardStyle(card, i)"
        >
          <CardView
            :game="game"
            :card="card"
            :playerId="playerId"
            :allowAbilityButtons="false"
            :allowInteractions="false"
            noOverlay
          />
          <!-- The back carries it too: it is an `img.card`, so the magnifier
               would happily blow up a card back mid-flip. -->
          <img class="card back no-overlay" :src="backFor(card)" />
        </div>
      </div>

      <button type="button" class="spotlight__ok" @click.stop="emit('dismiss')">
        {{ $t('ok') }}
      </button>
    </div>
  </div>
</template>

<style scoped>
/* Warm lamplight, deliberately not the mythos purple.
 *
 * The encounter reveal owns Indigo/Orchid, so borrowing it made a card you drew
 * for yourself read as something arriving to hurt you. Amber is the opposite
 * signal and stays clear of the meanings already spoken for: magenta is
 * `--select` ("the game is waiting on you"), teal is `--highlight` ("this
 * setting is off its default"). It sits a little warmer than the yellow-gold of
 * `--important` so a glow and a badge are not mistaken for each other.
 *
 * Authored in OKLCH so the four layers are one lightness ramp -- deep core,
 * bright mid, pale ambient, pulsing halo -- rather than four unrelated hex
 * guesses that drift in brightness as the hue moves. */
.spotlight {
  --spotlight-card-width: 300px;
  --spotlight-stagger: 0.3s;
  --spotlight-glow-core: oklch(46% 0.12 48);
  --spotlight-glow-mid: oklch(78% 0.15 72);
  /* The widest layer (150px spread) is the one that decides how much of the
     board goes foggy. Amber carries far more visible light than the indigo it
     replaced, so this sits well below the mid layer rather than above it. */
  --spotlight-glow-ambient: oklch(64% 0.11 70);
  --spotlight-glow-halo: oklch(74% 0.16 62);
  /* The halo dims rather than going black, so the pulse reads as a lamp
     guttering instead of a hole opening in the glow. */
  --spotlight-glow-halo-dim: oklch(24% 0.05 52);
  position: fixed;
  inset: 0;
  z-index: var(--z-index-1000);
  display: grid;
  place-content: center;
  color: white;
  text-align: center;
  filter: drop-shadow(0 0 3vmin var(--spotlight-glow-core))
    drop-shadow(0 5vmin 4vmin var(--spotlight-glow-mid))
    drop-shadow(2vmin -2vmin 15vmin var(--spotlight-glow-ambient))
    drop-shadow(0 0 7vmin var(--spotlight-glow-halo));
  animation:
    spotlight-in 0.3s ease-in-out,
    spotlight-glow 4s cubic-bezier(0.55, 0.085, 0.68, 0.53) infinite;
}

.spotlight__inner {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 10px;
}

.spotlight__title {
  font-family: Teutonic, serif;
  text-transform: uppercase;
  margin: 0;
  padding: 0;
  font-size: 2.5em;
  text-shadow: 0 2px 12px rgba(0, 0, 0, 0.9);
}

.spotlight__cards {
  display: flex;
  flex-direction: row;
  justify-content: center;
  width: fit-content;
}

/* Each card flips up from its own back. The per-card delay is an inline `--i`
 * rather than nth-child rules: the shared `--index` helper in base.css only
 * reaches 4, and a draw can be any size. */
.spotlight__card {
  position: relative;
  width: var(--spotlight-card-width);
  aspect-ratio: var(--card-aspect);
  perspective: 1000px;
  animation-delay: calc(var(--i) * var(--spotlight-stagger));
}

.spotlight__card .card.back {
  width: var(--spotlight-card-width);
  border-radius: 15px;
  transform-style: preserve-3d;
  position: absolute;
  top: 0;
  left: 0;
  backface-visibility: hidden;
  animation: spotlight-flip-back 0.3s linear;
  animation-fill-mode: forwards;
  animation-delay: inherit;
}

/* `:deep()` is kept OUT of a nested block deliberately. Nested, Vue emits the
   scope attribute as a bare `[data-v-x]`, which CSS nesting then reads as a
   descendant -- `.spotlight__card [data-v-x] .card-container` -- and nothing
   matches, because the card container IS the scoped child, not inside one. The
   rule silently did nothing: the front never flipped, it was simply already
   face-up while the back turned away over it. At top level the same `:deep()`
   compiles to `.spotlight__card[data-v-x] .card-container`, which is the
   intended selector. */
.spotlight__card :deep(.card-container) {
  transform: rotateY(-180deg);
  transform-style: preserve-3d;
  position: absolute;
  top: 0;
  left: 0;
  backface-visibility: hidden;
  animation: spotlight-flip-front 0.3s linear;
  animation-fill-mode: forwards;
  animation-delay: inherit;
}

.spotlight__card :deep(.card) {
  width: var(--spotlight-card-width) !important;
  min-width: 0 !important;
  aspect-ratio: var(--card-ratio);
  border-radius: 15px;
  margin: 0;
}

/* A fan overlaps the cards so any number of them fits. The overlap is derived
 * from the count rather than fixed, so the fan is always exactly
 * `--spotlight-fan-span` wide -- a fixed -62% fit four cards and pushed six off
 * both edges of a phone. The last card keeps its full width, so the newest draw
 * is the one you can actually read.
 *
 * Safe from a divide-by-zero: `:not(:last-child)` cannot match a single card,
 * and `.spotlight--fan` only exists above FAN_THRESHOLD. */
.spotlight--fan {
  --spotlight-card-width: min(240px, 42vw);
  --spotlight-fan-span: min(92vw, 880px);
  --spotlight-stagger: 0.08s;
}

.spotlight--fan .spotlight__card:not(:last-child) {
  margin-right: calc(
    -1 *
      (
        var(--spotlight-card-width) -
          (var(--spotlight-fan-span) - var(--spotlight-card-width)) / (var(--count) - 1)
      )
  );
}

.spotlight--fan .spotlight__card {
  transition: margin-right 120ms ease-out;
}

.spotlight--fan .spotlight__card:hover {
  z-index: 1;
}

/* Deep enough to read as lamplit wood rather than a warning, and tinted from
   the same ramp so the overlay stays one object instead of an amber glow around
   a purple modal button. */
.spotlight__ok {
  width: 100%;
  border: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: oklch(38% 0.08 55);
  font-weight: bold;
  color: #eee;
}

.spotlight__ok:hover {
  background-color: oklch(44% 0.1 58);
}

@keyframes spotlight-in {
  0% {
    opacity: 0;
    transform: scale(0.2);
  }
  65% {
    transform: scale(1.15);
  }
  100% {
    opacity: 1;
    transform: scale(1);
  }
}

@keyframes spotlight-glow {
  0%,
  100% {
    filter: drop-shadow(0 0 3vmin var(--spotlight-glow-core))
      drop-shadow(0 5vmin 4vmin var(--spotlight-glow-mid))
      drop-shadow(2vmin -2vmin 15vmin var(--spotlight-glow-ambient))
      drop-shadow(0 0 7vmin var(--spotlight-glow-halo));
  }
  50% {
    filter: drop-shadow(0 0 3vmin var(--spotlight-glow-core))
      drop-shadow(0 5vmin 4vmin var(--spotlight-glow-mid))
      drop-shadow(2vmin -2vmin 15vmin var(--spotlight-glow-ambient))
      drop-shadow(0 0 7vmin var(--spotlight-glow-halo-dim));
  }
}

@keyframes spotlight-flip-back {
  0% {
    opacity: 1;
    transform: rotateY(0deg);
  }
  49% {
    opacity: 1;
  }
  50% {
    opacity: 0;
  }
  100% {
    transform: rotateY(-180deg);
    opacity: 0;
  }
}

@keyframes spotlight-flip-front {
  0% {
    transform: rotateY(180deg);
    opacity: 0;
  }
  49% {
    opacity: 0;
  }
  50% {
    opacity: 1;
  }
  100% {
    opacity: 1;
    transform: rotateY(0deg);
  }
}

/* Someone who asked their system for less motion still gets the cards, at full
 * size, immediately -- just none of the choreography.
 *
 * The flip cannot simply be shortened. It is not a flourish laid over a visible
 * card: the front starts rotated away and hidden by `backface-visibility`, and
 * the animation is what brings it round. Removing the animation alone would
 * leave a card that never becomes visible, and a 0.01s duration still plays a
 * rotation, just too fast to follow. So the card is placed in its finished
 * state by hand and the back face is dropped outright. */
@media (prefers-reduced-motion: reduce) {
  .spotlight {
    animation: none;
  }

  .spotlight__card {
    animation: none;
    perspective: none;
    transition: none;
  }

  .spotlight__card :deep(.card-container) {
    animation: none;
    transform: none;
    opacity: 1;
  }

  .spotlight__card .card.back {
    display: none;
  }
}

@media (max-width: 800px) {
  .spotlight {
    --spotlight-card-width: min(220px, 55vw);
  }

  .spotlight__title {
    font-size: 1.5em;
  }

  .spotlight--fan {
    --spotlight-card-width: min(180px, 46vw);
    --spotlight-fan-span: 92vw;
  }
}
</style>
