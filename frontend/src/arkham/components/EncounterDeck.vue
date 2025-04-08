<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import * as ArkhamGame from '@/arkham/types/Game'
import { MessageType } from '@/arkham/types/Message'
import { useDebug } from '@/arkham/debug'

export interface Props {
  game: Game
  playerId: string
  spectral?: number
}

const isSpectral = computed(() => props.spectral !== undefined && props.spectral !== null)

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
    const { modifiers } = investigator?.value ?? { modifiers: [] }
    return modifiers.some((m) => m.type.tag === "OtherModifier" && m.type.contents === "TopCardOfEncounterDeckIsRevealed")
  })
})

const deckImage = computed(() => {
  if (revealTopCard.value) {
    let card = props.game.scenario.encounterDeck[0]
    if (card) {
      return imgsrc(`cards/${card.cardCode.replace('c', '')}.avif`)
    }
  }

  return imgsrc('back.png')
})

const deckAction = computed(() => {
  if (usingSpectral.value != isSpectral.value) {
    return -1;
  }

  return choices.value.findIndex((c) => c.tag === MessageType.TARGET_LABEL && (c.target.tag === "EncounterDeckTarget" || (isSpectral.value && c.target.tag === "ScenarioDeckTarget")))
})

const investigatorPortrait = computed(() => {
  const choice = choices.value[deckAction.value]

  if (!choice || !investigator.value) {
    return null;
  }

  if (investigator.value.form.tag === "HomunculusForm") {
    return imgsrc(`portraits/${investigator.value.id.replace('c', '')}.jpg`)
  }

  if (investigator.value.form.tag === "YithianForm") {
    return imgsrc(`portraits/${investigator.value.id.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${investigator.value.cardCode.replace('c', '')}.jpg`)
})

const deckLabel = computed(() => {
  if (isSpectral.value) {
    return "Spectral"
  }
  return null
})

const debug = useDebug()
</script>

<template>
  <div class="encounter-deck">
    <div class="top-of-deck">
      <img
        class="deck"
        :src="deckImage"
        :class="{ 'can-interact': deckAction !== -1, 'revealed': revealTopCard, 'card': revealTopCard }"
        @click="$emit('choose', deckAction)"
      />
      <span class="deck-size">{{props.spectral || game.encounterDeckSize}}</span>
      <span v-if="deckLabel" class="deck-label">{{deckLabel}}</span>
    </div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <template v-if="debug.active">
      <button @click="debug.send(game.id, {tag: 'InvestigatorDrawEncounterCard', contents: investigatorId})">Draw</button>
      <button @click="debug.send(game.id, {tag: 'FindAndDrawEncounterCard', contents: [investigatorId, {'tag': 'AnyCard', contents: []}, 'ExcludeDiscard']})">Select Draw</button>
      <button @click="debug.send(game.id, {tag: 'ShuffleDeck', contents: {'tag': 'EncounterDeck'}})">Shuffle</button>
    </template>
  </div>
</template>

<style scoped lang="scss">
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
  flex-direction: column;
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
</style>
