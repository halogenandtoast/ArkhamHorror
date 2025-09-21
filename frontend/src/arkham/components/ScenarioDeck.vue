<script lang="ts" setup>
import { computed, ComputedRef } from 'vue';
import { useDebug } from '@/arkham/debug';
import type { Card } from '@/arkham/types/Card';
import { imgsrc } from '@/arkham/helpers';
import { MessageType } from '@/arkham/types/Message'
import * as ArkhamGame from '@/arkham/types/Game'
import { Game } from '@/arkham/types/Game'

export interface Props {
  game: Game
  playerId: string
  deck: [string, Card[]]
}


const cards = computed(() => props.deck[1])
const debug = useDebug()
const props = defineProps<Props>()
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const emits = defineEmits<{
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
  choose: [value: number]
}>()

const choose = (idx: number) => emits('choose', idx)
const deckAction = computed(() => {
  return choices.value.findIndex((c) => c.tag === MessageType.TARGET_LABEL && c.target.tag === "ScenarioDeckTarget")
})

const showCards = () => emits('show', cards, props.deck[0], false)

const deckImage = computed(() => {
  switch(props.deck[0]) {
    case 'UnknownPlacesDeck':
      return imgsrc("cards/05134b.avif");
    case 'ExhibitDeck':
      return imgsrc("cards/02132b.avif");
    case 'CosmosDeck':
      return imgsrc("cards/05333b.avif");
    case 'CatacombsDeck':
      return imgsrc("cards/03247b.avif");
    case 'TidalTunnelDeck':
      return imgsrc("cards/07048b.avif");
    case 'TekeliliDeck':
      return imgsrc("player_back.jpg");
    case 'GuestDeck':
      return imgsrc("player_back.jpg");
    default:
      return imgsrc("back.png");
  }
})

const deckLabel = computed(() => {
  switch(props.deck[0]) {
    case 'CultistDeck':
      return "Cultists"
    case 'LunaticsDeck':
      return "Lunatics"
    case 'MonstersDeck':
      return "Monsters"
    case 'LeadsDeck':
      return "Leads"
    default:
      return null
  }
})
</script>

<template>
  <div class="deck">
    <img
      :src="deckImage"
      class="card"
      :class="{ 'can-interact': deckAction !== -1 }"
      @click="choose(deckAction)"
    />
    <span v-if="deckLabel" class="deck-label">{{deckLabel}}</span>
    <span class="deck-size">{{deck[1].length}}</span>
  </div>
  <button v-if="debug.active" @click="showCards">Show Cards</button>
</template>

<style scoped lang="scss">
.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: var(--card-width);
}

.deck {
  position: relative;
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

.deck-size {
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  width: 1.3em;
  height: 1.3em;
  border-radius: 1.3em;
  text-align: center;
  color: rgba(255, 255, 255, 0.7);
  background-color: rgba(0, 0, 0, 0.8);
  left: 50%;
  bottom: 0%;
  transform: translateX(-50%) translateY(-50%);
  pointer-events: none;
}

.can-interact {
  border: 3px solid var(--select);
  cursor: pointer;
}
</style>
