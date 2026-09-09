<script lang="ts" setup>
import { computed, ref } from 'vue'
import { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { cardImage, investigatorPortrait as portraitFor } from '@/arkham/cardImages'
import * as ArkhamGame from '@/arkham/types/Game'
import { MessageType } from '@/arkham/types/Message'
import { useDebug } from '@/arkham/debug'
import * as Api from '@/arkham/api'
import * as CardT from '@/arkham/types/Card'
import type { CardDef } from '@/arkham/types/CardDef'
import { fullName } from '@/arkham/types/Name'
import { useCardStore } from '@/stores/cards'
import * as DebugMove from '@/arkham/debugCardMove'

export interface Props {
  game: Game
  playerId: string
  spectral?: number
}

const isSpectral = computed(() => props.spectral !== undefined && props.spectral !== null)
const deckKey = computed(() => {
  if (isSpectral.value) return "SpectralEncounterDeck"
  return "RegularEncounterDeck"
})

const deckSignifier = computed(() => ({ tag: "EncounterDeckByKey", contents: deckKey.value }))

const props = defineProps<Props>()
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const investigator = computed(() => {
  return Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
})

const investigatorId = computed(() => {
  return investigator.value ? investigator.value.id : "00000"
})

const usingSpectral = computed(() => {
  const { modifiers } = investigator?.value ?? { modifiers: [] }
  return modifiers ? modifiers.some((m) => m.type.tag === "UseEncounterDeck" && m.type.contents === "SpectralEncounterDeck") : false
})

const revealTopCard = computed(() => {
  return Object.values(props.game.investigators).some((i) => {
    const { modifiers } = i
    return (modifiers ?? []).some((m) => m.type.tag === "OtherModifier" && m.type.contents === "TopCardOfEncounterDeckIsRevealed")
  })
})

const deckImage = computed(() => {
  if (revealTopCard.value) {
    let card = props.game.scenario?.encounterDeck[0]
    if (card) {
      return cardImage(card.cardCode)
    }
  }

  return imgsrc('backs/back_encounter.jpg')
})

const deckAction = computed(() => {
  if (usingSpectral.value != isSpectral.value) {
    return -1;
  }

  return choices.value.findIndex((c) => c.tag === MessageType.TARGET_LABEL && (c.target.tag === "EncounterDeckTarget" || (isSpectral.value && c.target.tag === "ScenarioDeckTarget")))
})

const investigatorPortrait = computed(() => {
  const choice = choices.value[deckAction.value]
  if (!choice || !investigator.value) return null
  return portraitFor(props.game, investigator.value.id)
})

const deckLabel = computed(() => {
  if (isSpectral.value) {
    return "Spectral"
  }
  return null
})

const cardStore = useCardStore()

// null while nothing is being dragged, false when the card in flight has the
// wrong back (or two of them) for an encounter deck.
const deckAccepts = computed(() => DebugMove.draggedCardAccepted(props.game, cardStore.cards, 'encounterDeck'))
const draggedOver = ref(false)

const dragover = (e: DragEvent) => {
  e.preventDefault()
  draggedOver.value = true
  if (e.dataTransfer) {
    // The drag sources declare effectAllowed 'copy'; answering with a dropEffect
    // outside that set makes the browser refuse the drop outright.
    e.dataTransfer.dropEffect = deckAccepts.value === false ? 'none' : 'copy'
  }
}

function onDragLeave(event: DragEvent) {
  const target = event.currentTarget
  const related = event.relatedTarget
  if (target instanceof Node && related instanceof Node && target.contains(related)) return
  draggedOver.value = false
}

function onDrop(event: DragEvent) {
  event.preventDefault()
  draggedOver.value = false
  if (!event.dataTransfer) return
  const data = event.dataTransfer.getData('text/plain')
  if (!data) return
  const json = JSON.parse(data)
  if (json.tag === "EnemyTarget") {
    debug.send(props.game.id, {tag: 'ShuffleIntoDeck', contents: [deckSignifier.value, json]})
    return
  }
  if (json.tag !== "CardTarget") return
  const card = DebugMove.resolveCard(props.game, json.contents)
  if (!card) return
  if (!DebugMove.canMoveCardTo(DebugMove.cardDefFor(cardStore.cards, card), 'encounterDeck')) return
  DebugMove.debugMoveCard(
    props.game.id,
    json.contents,
    DebugMove.toDeck(deckSignifier.value as DebugMove.DeckSignifier, 'DebugDeckShuffle'),
  )
}

const debug = useDebug()

function drawEncounterCard() {
  debug.send(props.game.id, {tag: 'DrawCards', contents: [investigatorId.value, {cardDrawSource: {tag: 'GameSource'}, cardDrawDeck: deckSignifier.value, cardDrawAmount: 1, cardDrawState: {tag: 'UnresolvedCardDraw'}, cardDrawTarget: null, cardDrawAction: false, cardDrawKind: 'StandardCardDraw', cardDrawRules: [], cardDrawAndThen: null, cardDrawAlreadyDrawn: [], cardDrawDiscard: null}]})
}

function discardEncounterCards(amount: number) {
  debug.send(props.game.id, {tag: 'DiscardUntilN', contents: [amount, investigatorId.value, {tag: 'GameSource'}, {tag: 'GameTarget'}, deckSignifier.value, {tag: 'AnyCard', contents: []}]})
}

function selectAndDrawEncounterCard() {
  debug.send(props.game.id, {tag: 'FindAndDrawEncounterCardWithDeckKey', contents: [investigatorId.value, {'tag': 'AnyCard', contents: []}, 'ExcludeDiscard', deckKey.value]})
}

function shuffleEncounterDeck() {
  debug.send(props.game.id, {tag: 'ShuffleDeck', contents: deckSignifier.value})
}

const showDebugAddCard = ref(false)
const debugEncounterCards = ref<CardDef[]>([])
const debugCardSearch = ref('')
const debugAddCardError = ref<string | null>(null)
const debugAddCardLoading = ref(false)

// Act/agenda/scenario/investigator cards can never sit in an encounter deck.
const debugCardTypes = new Set([
  'EnemyType',
  'TreacheryType',
  'LocationType',
  'EncounterAssetType',
  'EncounterEventType',
  'StoryType',
])

function debugCardCode(card: CardDef) {
  return card.cardCode.replace(/^c/, '')
}

function debugCardLabel(card: CardDef) {
  return `${fullName(card.name)} [${debugCardCode(card)}]`
}

// Every encounter card code the scenario has put anywhere on the table, used to
// work out which encounter sets are in play so the unsearched list is useful.
const scenarioCardCodes = computed(() => {
  const scenario = props.game.scenario
  if (!scenario) return new Set<string>()

  const decks = Object.values(scenario.encounterDecks).flatMap(([deck, discard]) => [...deck, ...discard])

  return new Set([
    ...scenario.encounterDeck.map((c) => c.cardCode),
    ...scenario.discard.map((c) => c.cardCode),
    ...decks.map((c) => c.cardCode),
    ...scenario.victoryDisplay.map(CardT.asCardCode),
    ...scenario.setAsideCards.map(CardT.asCardCode),
  ])
})

const scenarioEncounterSets = computed(() => {
  const codes = scenarioCardCodes.value
  return new Set(
    debugEncounterCards.value
      .filter((card) => codes.has(card.cardCode))
      .map((card) => card.encounterSet)
      .filter((set) => set != null),
  )
})

function isOutsideScenarioSets(card: CardDef) {
  const sets = scenarioEncounterSets.value
  return sets.size > 0 && !sets.has(card.encounterSet)
}

const filteredDebugEncounterCards = computed(() => {
  const query = debugCardSearch.value.trim().toLocaleLowerCase()
  const cards = [...debugEncounterCards.value].sort((a, b) =>
    debugCardLabel(a).localeCompare(debugCardLabel(b)),
  )

  if (!query) {
    const sets = scenarioEncounterSets.value
    const inScenario = cards.filter((card) => sets.has(card.encounterSet))
    return (inScenario.length > 0 ? inScenario : cards).slice(0, 50)
  }

  return cards
    .filter((card) => {
      const haystack = [
        card.cardCode,
        fullName(card.name),
        card.cardType,
        card.encounterSet ?? '',
        ...card.cardTraits,
      ]
        .join(' ')
        .toLocaleLowerCase()

      return haystack.includes(query)
    })
    .slice(0, 50)
})

async function openDebugAddCard() {
  if (!debug.active) return
  showDebugAddCard.value = true
  debugAddCardError.value = null

  if (debugEncounterCards.value.length === 0) {
    debugAddCardLoading.value = true
    try {
      const allCards = await Api.fetchCards('campaign')
      debugEncounterCards.value = allCards.filter(
        (card) => card.encounterSet != null && debugCardTypes.has(card.cardType),
      )
    } catch (error) {
      console.error(error)
      debugAddCardError.value = 'Unable to load encounter cards.'
    } finally {
      debugAddCardLoading.value = false
    }
  }
}

async function debugAddCardToDeck(card: CardDef) {
  debugAddCardError.value = null
  const cardId = crypto.randomUUID()

  try {
    await debug.send(props.game.id, { tag: 'CreateCard', contents: [cardId, card.cardCode] })
    await debug.send(props.game.id, {
      tag: 'DebugAddToEncounterDeck',
      contents: [deckSignifier.value, cardId],
    })
    debugCardSearch.value = ''
    showDebugAddCard.value = false
  } catch (error) {
    console.error(error)
    debugAddCardError.value = `Unable to add ${fullName(card.name)} to the encounter deck.`
  }
}

</script>

<template>
  <div class="encounter-deck">
    <div v-if="debug.active" class="debug-buttons">
      <button @click="drawEncounterCard">{{ $t('encounterDeck.draw') }}</button>
      <button
        @click.exact="discardEncounterCards(1)"
        @click.shift="discardEncounterCards(5)"
      >{{ $t('treachery.discard') }}</button>
      <button @click="selectAndDrawEncounterCard">{{ $t('draw.selectDraw') }}</button>
      <button @click="shuffleEncounterDeck">{{ $t('draw.shuffle') }}</button>
      <button @click="openDebugAddCard">+ Card to deck</button>
    </div>

    <Teleport to="body">
      <div
        v-if="debug.active && showDebugAddCard"
        class="debug-add-card-overlay no-card-overlay"
        @click.self="showDebugAddCard = false"
      >
        <div class="debug-add-card-modal">
          <h3>Shuffle an encounter card into the {{ isSpectral ? 'spectral ' : '' }}encounter deck</h3>
          <p class="debug-add-card-warning">
            Cards from outside the scenario's own encounter sets can reference locations,
            enemies, or campaign state that this scenario never set up, and may break the game.
          </p>
          <label>
            Search card
            <input
              v-model="debugCardSearch"
              type="search"
              autofocus
              placeholder="Name, code, type, encounter set, or trait"
              @keydown.stop
            />
          </label>
          <p v-if="debugAddCardLoading" class="debug-add-card-status">Loading encounter cards…</p>
          <p v-if="debugAddCardError" class="debug-add-card-error">{{ debugAddCardError }}</p>
          <div v-else class="debug-add-card-results">
            <button
              v-for="card in filteredDebugEncounterCards"
              :key="card.cardCode"
              type="button"
              :data-image-id="debugCardCode(card)"
              @click="debugAddCardToDeck(card)"
            >
              <span>
                {{ debugCardLabel(card) }}
                <em v-if="isOutsideScenarioSets(card)" class="debug-add-card-foreign">not in this scenario</em>
              </span>
              <small>{{ card.cardType }} · {{ card.encounterSet }}</small>
            </button>
          </div>
          <button type="button" @click="showDebugAddCard = false">{{ $t('close') }}</button>
        </div>
      </div>
    </Teleport>

    <div class="deck-area">
      <div class="top-of-deck">
        <img
          class="deck"
          :src="deckImage"
          :class="{ 'can-interact': deckAction !== -1, 'revealed': revealTopCard, 'card': revealTopCard, 'deck--drop-target': draggedOver && deckAccepts === true, 'deck--drop-refused': draggedOver && deckAccepts === false }"
          @click="$emit('choose', deckAction)"
          @drop="onDrop($event)"
          @dragover.prevent="dragover($event)"
          @dragleave="onDragLeave($event)"
          @dragend="draggedOver = false"
          @dragenter.prevent
        />
        <span class="deck-size">{{props.spectral === undefined ? game.encounterDeckSize : props.spectral}}</span>
        <span v-if="deckLabel" class="deck-label">{{deckLabel}}</span>
      </div>
      <img
        v-if="investigatorPortrait"
        class="portrait"
        :src="investigatorPortrait"
      />
    </div>
  </div>
</template>

<style scoped>
/* Pending drop target — the receiver of the drag, so cyan, matching
   `cards-under-indicator--dragged-over` rather than the magenta reserved for
   choices the game is awaiting. Refused drops get a plain red: an error state,
   not a role in the highlight language. */
.deck--drop-target {
  outline: 3px solid var(--highlight);
  outline-offset: 3px;
}

.deck--drop-refused {
  outline: 3px solid rgba(220, 70, 70, 0.9);
  outline-offset: 3px;
}

.revealed {
  filter: brightness(50%);
}
.deck {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  width: var(--card-width);
}
.can-interact {
  border: 3px solid var(--select);
  cursor: pointer;
}

.encounter-deck {
  display: flex;
  align-items: flex-start;
  gap: 0.35rem;
  position: relative;
}

.deck-area {
  position: relative;
}

.top-of-deck {
  position: relative;
  width: fit-content;
}

.deck-size {
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 0.6);
  left: 50%;
  bottom: 0%;
  transform: translateX(-50%) translateY(-50%);
  pointer-events: none;
}

.portrait {
  width: calc(var(--card-width) * 0.55);
  position: absolute;
  opacity: 0.8;
  border-radius: 5px;
  left: 50%;
  top: 10%;
  transform: translateX(-50%);
  pointer-events: none;
}

.deck-label {
  position: absolute;
  top: 0;
  left: 50%;
  font-weight: bold;
  border-radius: 3px;
  padding: 0 2px;
  transform: translateX(-50%) translateY(50%);
  background: rgba(255,255,255,0.8);
}

.debug-buttons {
  display: flex;
  flex-direction: column;
  align-items: stretch;
  gap: 0.25rem;
  width: max-content;
  max-width: 9rem;
}

.debug-buttons button {
  font-size: 0.75rem;
  white-space: nowrap;
}

.debug-add-card-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  /* Under the card hover preview, so hovering a result shows its card art on
   * top of the modal instead of behind it. */
  z-index: calc(var(--z-card-hover-overlay) - 1);
}

.debug-add-card-modal {
  background: #1a1a2e;
  border: 1px solid var(--button-highlight);
  border-radius: 8px;
  color: #eee;
  max-width: 700px;
  min-width: 300px;
  padding: 1.5rem;
  width: min(700px, 90vw);

  h3 {
    color: #adf;
    font-size: 1.1rem;
    margin: 0 0 1rem;
  }

  label {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
    margin-bottom: 0.75rem;
  }

  input {
    background: #111827;
    border: 1px solid #4b5563;
    border-radius: 4px;
    color: #eee;
    padding: 0.5rem;
  }
}

.debug-add-card-results {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  max-height: 50vh;
  overflow: auto;

  button {
    align-items: flex-start;
    background: rgba(255, 255, 255, 0.06);
    border: 1px solid transparent;
    color: #eee;
    cursor: pointer;
    display: flex;
    flex-direction: column;
    gap: 0.15rem;
    margin: 0;
    padding: 0.5rem;
    text-align: left;

    &:hover {
      background: rgba(255, 255, 255, 0.12);
      border-color: var(--button-highlight);
    }
  }

  small {
    opacity: 0.75;
  }
}

.debug-add-card-warning {
  background: rgba(212, 165, 10, 0.12);
  border-left: 3px solid rgba(212, 165, 10, 0.9);
  border-radius: 3px;
  color: #f0d999;
  font-size: 0.85rem;
  margin-bottom: 0.75rem;
  padding: 0.5rem 0.6rem;
}

.debug-add-card-foreign {
  color: #f0d999;
  font-size: 0.75rem;
  font-style: normal;
  margin-left: 0.4rem;
  opacity: 0.9;
}

.debug-add-card-error {
  color: #f88;
}

.debug-add-card-status {
  opacity: 0.8;
}
</style>
