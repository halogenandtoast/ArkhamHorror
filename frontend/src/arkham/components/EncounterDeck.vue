<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers'
import * as ArkhamGame from '@/arkham/types/Game'
import { MessageType } from '@/arkham/types/Message'
import { useDebug } from '@/arkham/debug'

export interface Props {
  game: Game
  investigatorId: string
  spectral?: number
}

const isSpectral = computed(() => props.spectral !== undefined && props.spectral !== null)

const props = defineProps<Props>()
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const usingSpectral = computed(() => {
  const { modifiers } = props.game.investigators[props.investigatorId]
  return modifiers ? modifiers.some((m) => m.type.tag === "UseEncounterDeck" && m.type.contents === "SpectralEncounterDeck") : false
})

const deckAction = computed(() => {
  if (usingSpectral.value != isSpectral.value) {
    return -1;
  }

  return choices.value.findIndex((c) => c.tag === MessageType.TARGET_LABEL && (c.target.tag === "EncounterDeckTarget" || (isSpectral.value && c.target.tag === "ScenarioDeckTarget")))
})

const investigatorPortrait = computed(() => {
  const choice = choices.value[deckAction.value]

  if (!choice) {
    return null;
  }

  const player = props.game.investigators[props.investigatorId]

  if (player.isYithian) {
    return imgsrc(`portraits/${props.investigatorId.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
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
        :src="imgsrc('back.png')"
        :class="{ 'can-interact': deckAction !== -1 }"
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
    </template>
  </div>
</template>

<style scoped lang="scss">
.deck {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: $card-width;
}
.can-interact {
  border: 3px solid $select;
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
  width: $card-width * 0.55;
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
