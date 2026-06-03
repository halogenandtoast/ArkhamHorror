<script lang="ts" setup>
import { computed, ComputedRef } from 'vue';
import { useDebug } from '@/arkham/debug';
import type { Card } from '@/arkham/types/Card';
import { cardImage } from '@/arkham/types/Card';
import { imgsrc } from '@/arkham/helpers';
import { MessageType } from '@/arkham/types/Message'
import * as ArkhamGame from '@/arkham/types/Game'
import { Game } from '@/arkham/types/Game'

export interface Props {
  game: Game
  playerId: string
  deck: [string, Card[]]
  discardPile?: Card[]
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
  return choices.value.findIndex((c) => {
    if (c.tag !== MessageType.TARGET_LABEL) return false
    if (props.deck[0] === 'AbyssDeck') return c.target.tag === "EncounterDeckTarget"
    return c.target.tag === "ScenarioDeckTarget"
  })
})

const revealedCards = computed(() => props.deck[1].map(card => {
  if (card.tag === 'EncounterCard') {
    return { ...card, contents: { ...card.contents, isFlipped: false } }
  }
  return card
}))
const showCards = () => emits('show', revealedCards, props.deck[0], false)

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
    case 'OtherworldDeck':
      let topCard = props.deck[1][0];
      if (topCard) {
        return imgsrc(cardImage(topCard));
      }
    case 'WoodsDeck':
      return imgsrc("cards/10612b.avif");
    case 'CavernsDeck':
      return imgsrc("cards/10577b.avif");
    case 'EnemyDeck':
      return imgsrc("backs/back_the_longest_night.jpg");
    case 'AbyssDeck':
      return imgsrc("cards/10670b.avif");
    default:
      return imgsrc("back.png");
  }
})

const topOfDiscard = computed(() => {
  if (!props.discardPile || props.discardPile.length === 0) return null
  return props.discardPile[0]
})

const topOfDiscardImage = computed(() => {
  if (!topOfDiscard.value) return null
  return imgsrc(cardImage(topOfDiscard.value))
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
  <div class="scenario-deck-area">
    <div v-if="topOfDiscard" class="discard-card">
      <img :src="topOfDiscardImage ?? undefined" class="card" />
      <span class="deck-size">{{ discardPile!.length }}</span>
    </div>
    <div class="deck">
      <img
        :src="deckImage"
        class="card"
        :class="{ 'can-interact': deckAction !== -1 }"
        @click="choose(deckAction)"
      />
      <span v-if="deckLabel" class="deck-label">{{deckLabel}}</span>
      <span class="deck-size" :class="{ 'abyss-deck-size': deck[0] === 'AbyssDeck' }">{{deck[1].length}}</span>
    </div>
    <button v-if="debug.active" @click="showCards">{{ $t('scenarioDeck.showCards') }}</button>
  </div>
</template>

<style scoped>
.scenario-deck-area {
  display: flex;
  gap: 2px;
}

.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: var(--card-width);
}

.deck {
  position: relative;
}

.discard-card {
  position: relative;
  width: fit-content;
  line-height: 0;
  height: min-content;

  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  .card {
    box-shadow: unset;
    margin: 0;
    display: block;
  }
  .deck-size {
    z-index: 1;
    width: auto;
    height: auto;
    border-radius: 0;
    background-color: transparent;
    color: rgba(255, 255, 255, 1);
    bottom: 55%;
    -webkit-text-stroke: 1px black;
  }
  &::after {
    border-radius: 6px;
    pointer-events: none;
    content: "";
    position: absolute;
    inset: 0;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
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

.abyss-deck-size {
  top: 6px;
  right: 6px;
  left: auto;
  bottom: auto;
  transform: none;
  min-width: 1.7em;
  width: auto;
  height: 1.7em;
  line-height: 1.7em;
  padding: 0 0.35em;
  border-radius: 999px;
  font-size: 0.95em;
  color: white;
  background: rgba(0, 0, 0, 0.75);
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.55);
}

.can-interact {
  border: 3px solid var(--select);
  cursor: pointer;
}
</style>
