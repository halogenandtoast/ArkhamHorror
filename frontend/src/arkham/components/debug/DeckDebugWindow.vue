<script lang="ts" setup>
/* The whole deck already reaches the client in order, so "look at the top X"
 * needs no engine search: it snapshots the ids and lists whichever are still in
 * the deck. Moving one away does not slide the next card into view. */
import { computed, ref } from 'vue'
import Draggable from '@/components/Draggable.vue'
import { useDebug } from '@/arkham/debug'
import { useEscape } from '@/composable/escape'
import { imgsrc } from '@/arkham/helpers'
import { cardImage, type CardContents } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import type * as Arkham from '@/arkham/types/Investigator'
import * as DebugMove from '@/arkham/debugCardMove'
import { drawCardsMessage } from '@/arkham/debugPlayer'

const props = defineProps<{
  game: Game
  investigator: Arkham.Investigator
}>()

const emit = defineEmits<{ close: [] }>()

useEscape(() => emit('close'))

const debug = useDebug()
const iid = computed(() => props.investigator.id)
const deck = computed(() => props.investigator.deck)
const deckSignifier = computed<DebugMove.DeckSignifier>(() => ({ tag: 'InvestigatorDeck', contents: iid.value }))

const clamp = (n: number, min: number, max: number) => Math.min(max, Math.max(min, Math.floor(n || 0)))

/* ---- Draw ------------------------------------------------------------------ */

const drawCount = ref(1)
const stepDraw = (by: 1 | -1) => { drawCount.value = clamp(drawCount.value + by, 1, 20) }
const draw = () => debug.send(props.game.id, drawCardsMessage(iid.value, drawCount.value))

/* ---- Select draw / shuffle ------------------------------------------------- */

// Only one search at a time: a second would clobber the first's found cards.
const canSelectDraw = computed(() => Object.keys(props.investigator.foundCards).length === 0)

// The search puts a card picker in front of the player, so get out of its way.
const selectDraw = () => {
  emit('close')
  debug.send(props.game.id, {
    tag: 'SearchMessage',
    contents: {
      tag: 'Search_',
      contents: [
        'Looking',
        iid.value,
        { tag: 'GameSource', contents: [] },
        { tag: 'InvestigatorTarget', contents: iid.value },
        [[{ tag: 'FromDeck', contents: [] }, 'ShuffleBackIn']],
        { tag: 'BasicCardMatch', contents: { tag: 'AnyCard', contents: [] } },
        { tag: 'DrawFound', contents: [iid.value, 1] },
      ],
    },
  })
}

const shuffle = () => debug.send(props.game.id, { tag: 'ShuffleDeck', contents: deckSignifier.value })

/* ---- Search the top X ------------------------------------------------------ */

const searchCount = ref(3)
const lookedAt = ref<string[] | null>(null)

const look = () => {
  searchCount.value = clamp(searchCount.value, 1, Math.max(1, deck.value.length))
  lookedAt.value = deck.value.slice(0, searchCount.value).map((card) => card.id)
}

const lookedAtCards = computed<{ card: CardContents, position: number }[]>(() => {
  const ids = lookedAt.value
  if (!ids) return []
  return deck.value
    .map((card, index) => ({ card, position: index + 1 }))
    .filter(({ card }) => ids.includes(card.id))
})

const move = (card: CardContents, destination: DebugMove.DebugCardDestination) =>
  DebugMove.debugMoveCard(props.game.id, card.id, destination)

const toHand = (card: CardContents) => move(card, DebugMove.toHand(iid.value))
const toDiscard = (card: CardContents) => move(card, DebugMove.discarded)
const toTop = (card: CardContents) => move(card, DebugMove.toDeck(deckSignifier.value, 'DebugDeckTop'))
const toBottom = (card: CardContents) => move(card, DebugMove.toDeck(deckSignifier.value, 'DebugDeckBottom'))
const removeFromGame = (card: CardContents) => move(card, DebugMove.removedFromGame)

const image = (card: CardContents) => imgsrc(cardImage(card))
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.deck.title') }}</h2></template>
    <div class="debug-modal debug-window deck-debug">
      <p class="deck-debug__size">{{ $t('debug.deck.size', { count: deck.length }) }}</p>

      <section class="deck-debug__section">
        <span class="deck-debug__label">{{ $t('debug.deck.draw') }}</span>
        <div class="deck-debug__row">
          <div class="deck-debug__stepper">
            <button type="button" @click="stepDraw(-1)">−</button>
            <span>{{ drawCount }}</span>
            <button type="button" @click="stepDraw(1)">+</button>
            <button type="button" class="deck-debug__stepper-draw" :disabled="deck.length === 0" @click="draw">{{ $t('debug.deck.drawButton') }}</button>
          </div>
          <button type="button" :disabled="!canSelectDraw || deck.length === 0" v-tooltip="$t('debug.deck.selectDrawHint')" @click="selectDraw">
            {{ $t('draw.selectDraw') }}
          </button>
          <button type="button" @click="shuffle">{{ $t('draw.shuffle') }}</button>
        </div>
      </section>

      <section class="deck-debug__section">
        <span class="deck-debug__label">{{ $t('debug.deck.search') }}</span>
        <div class="deck-debug__row">
          <label class="deck-debug__field">
            <span>{{ $t('debug.deck.topX') }}</span>
            <input v-model.number="searchCount" type="number" min="1" :max="deck.length" />
          </label>
          <button type="button" :disabled="deck.length === 0" @click="look">{{ $t('debug.deck.look') }}</button>
          <button v-if="lookedAt" type="button" @click="lookedAt = null">{{ $t('debug.deck.done') }}</button>
        </div>
        <p v-if="lookedAt && lookedAtCards.length === 0" class="deck-debug__note">{{ $t('debug.deck.allMoved') }}</p>
        <ul v-else-if="lookedAt" class="deck-debug__cards">
          <li v-for="{ card, position } in lookedAtCards" :key="card.id" class="deck-debug__card">
            <span class="count-pill deck-debug__position" v-tooltip="$t('debug.deck.positionHint')">{{ position }}</span>
            <img :src="image(card)" class="card-no-overlay" :data-image-id="card.cardCode" />
            <div class="deck-debug__actions">
              <button type="button" @click="toHand(card)">{{ $t('debug.deck.toHand') }}</button>
              <button type="button" @click="toDiscard(card)">{{ $t('debug.deck.toDiscard') }}</button>
              <button type="button" @click="toTop(card)">{{ $t('debug.deck.toTop') }}</button>
              <button type="button" @click="toBottom(card)">{{ $t('debug.deck.toBottom') }}</button>
              <button type="button" class="deck-debug__danger" @click="removeFromGame(card)">{{ $t('debug.deck.remove') }}</button>
            </div>
          </li>
        </ul>
      </section>

      <button class="debug-close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
.deck-debug {
  max-width: min(720px, 92vw);
  color: var(--text);
  font-size: 0.85rem;
}

.deck-debug__size {
  margin: 0;
  color: rgba(255, 255, 255, 0.7);
}

.deck-debug__section {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px 12px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
}

.deck-debug__label {
  font-family: teutonic, sans-serif;
  font-size: 0.95rem;
  letter-spacing: 0.04em;
  color: rgba(255, 255, 255, 0.72);
}

.deck-debug__row {
  display: flex;
  flex-flow: row wrap;
  align-items: center;
  gap: 6px;
}

.deck-debug__note {
  margin: 0;
  color: rgba(255, 255, 255, 0.55);
}

.deck-debug button {
  min-width: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  font-size: 0.8rem;
  padding: 6px 10px;
  cursor: pointer;
  transition: background-color 0.12s ease, border-color 0.12s ease;
}

.deck-debug button:hover:not(:disabled) {
  background: var(--button);
  border-color: var(--select);
}

.deck-debug button:disabled {
  opacity: 0.45;
  cursor: not-allowed;
}

.deck-debug button.deck-debug__danger:hover:not(:disabled) {
  background: #7a2020;
  border-color: #c05050;
}

.deck-debug .debug-close {
  width: 100%;
}

/* − count + Draw as one control: a single border, with the buttons as its segments */
.deck-debug__stepper {
  display: inline-flex;
  align-self: stretch;
  align-items: stretch;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  overflow: hidden;
}

.deck-debug__stepper span {
  display: flex;
  align-items: center;
  justify-content: center;
  min-width: 1.8em;
  font-weight: 600;
  border-inline: 1px solid rgba(255, 255, 255, 0.15);
}

.deck-debug__stepper button {
  width: 26px;
  padding: 0;
  border: 0;
  border-radius: 0;
  background: transparent;
}

.deck-debug__stepper button:hover:not(:disabled) {
  border-color: transparent;
}

.deck-debug__stepper .deck-debug__stepper-draw {
  width: auto;
  padding: 0 10px;
  border-left: 1px solid rgba(255, 255, 255, 0.15);
}

.deck-debug__stepper .deck-debug__stepper-draw:hover:not(:disabled) {
  border-left-color: rgba(255, 255, 255, 0.15);
}

.deck-debug__field {
  display: inline-flex;
  align-items: center;
  gap: 6px;
}

.deck-debug__field input {
  width: 4.5em;
  padding: 4px 6px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
}

.deck-debug__cards {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.deck-debug__card {
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  width: 120px;
}

.deck-debug__card img {
  width: 100%;
  height: auto;
  border-radius: 5px;
}

.deck-debug__position {
  position: absolute;
  top: 4px;
  left: 4px;
}

.deck-debug__actions {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 4px;
}

.deck-debug__actions button {
  padding: 3px 6px;
  font-size: 0.72rem;
}
</style>
