<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import { cardImage, investigatorPortrait as portraitFor } from '@/arkham/cardImages'
import * as ArkhamGame from '@/arkham/types/Game'
import { MessageType } from '@/arkham/types/Message'
import { useDebug } from '@/arkham/debug'

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

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'move'
  }
}

function onDrop(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "EnemyTarget") {
        debug.send(props.game.id, {tag: 'ShuffleIntoDeck', contents: [deckSignifier.value, json]})
      }
    }
  }
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
    </div>
    <div class="deck-area">
      <div class="top-of-deck">
        <img
          class="deck"
          :src="deckImage"
          :class="{ 'can-interact': deckAction !== -1, 'revealed': revealTopCard, 'card': revealTopCard }"
          @click="$emit('choose', deckAction)"
          @drop="onDrop($event)"
          @dragover.prevent="dragover($event)"
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
</style>
