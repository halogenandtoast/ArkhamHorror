<script lang="ts" setup>   
import { useDebug } from '@/arkham/debug'
import * as ArkhamCard from '@/arkham/types/Card';
import * as Arkham from '@/arkham/types/Investigator'
import * as ArkhamGame from '@/arkham/types/Game';
import type { Game } from '@/arkham/types/Game'
import {computed, ComputedRef, ref, reactive} from 'vue'
import { imgsrc, pluralize } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';
import Card from '@/arkham/components/Card.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import CardRow from '@/arkham/components/CardRow.vue';

const { t } = useI18n();

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

export interface Props {
  game: Game
  investigator: Arkham.Investigator
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const investigatorId = computed(() => props.investigator.id)
const debug = useDebug()

const id = computed(() => props.investigator.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const topOfDiscard = computed(() => discards.value[0])

const topOfDeckRevealed = computed(() =>
  props.investigator.modifiers?.some((m) => m.type.tag === "OtherModifier" && m.type.contents === "TopCardOfDeckIsRevealed")
)

const topOfDeck = computed(() => {
  const topCard = props.investigator.deck[0]
  if  (topOfDeckRevealed.value && topCard) {
    return imgsrc(`cards/${topCard.cardCode.replace(/^c/, '')}.avif`)
  }
  return imgsrc("player_back.jpg")
})

const playTopOfDeckAction = computed(() => {
  if(props.playerId !== props.investigator.playerId) {
    return -1
  }
  const topOfDeck = props.investigator.deck[0]
  if (topOfDeck !== undefined && topOfDeck !== null && topOfDeckTreachery.value === null) {
    return choices.value.findIndex((c) => c.tag === "TargetLabel" && c.target.contents === props.investigator.deck[0].id)
  }
  return -1
})

const viewingDiscard = ref(false)
const viewDiscardLabel = computed(() => viewingDiscard.value ? t('close') : pluralize(t('scenario.discardCard'), discards.value.length))

const drawCardsAction = computed(() => {
  if(props.playerId !== props.investigator.playerId) {
    return -1
  }
  return choices
    .value
    .findIndex((c) => {
      if (c.tag === "ComponentLabel") {
        return (c.component.tag == "InvestigatorDeckComponent")
      }
      return false
    });
})

const noCards = computed<ArkhamCard.Card[]>(() => [])

// eslint-disable-next-line
const showCards = reactive<RefWrapper<any>>({ ref: noCards })
const cardRowTitle = ref("")

const topOfDeckTreachery = computed(() => {
  const mTreacheryId = Object.values(props.game.treacheries).
    filter((t) => t.placement.tag === "OnTopOfDeck" && t.placement.contents === id.value).
    map((t) => t.id)[0]
  return mTreacheryId ? props.game.treacheries[mTreacheryId] : null
})

const hideCards = () => {
  showCards.ref = noCards
  viewingDiscard.value = false
}

function onDropDiscard(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "CardTarget") {
        debug.send(props.game.id, {tag: 'DiscardCard', contents: [id.value, {'tag': 'GameSource' }, json.contents]})
      }
    }
  }
}

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'copy'
  }
}

const doShowCards = (event: Event, cards: ComputedRef<ArkhamCard.Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}
const showDiscards = (e: Event) => doShowCards(e, discards, t('investigator.discards'), true)
const discards = computed<ArkhamCard.Card[]>(() => props.investigator.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))

</script>

<template>
  <div class="discard"
    @drop="onDropDiscard($event)"
    @dragover.prevent="dragover($event)"
    @dragenter.prevent
    @click="showDiscards"
  >
    <Card v-if="topOfDiscard" :game="game" :card="topOfDiscard" :playerId="playerId" @choose="emit('choose', $event)" />
    <button v-if="discards.length > 0" class="view-discard-button" @click="showDiscards">{{viewDiscardLabel}}</button>
    <button v-if="debug.active && discards.length > 0" class="view-discard-button" @click="debug.send(game.id, {tag: 'ShuffleDiscardBackIn', contents: investigatorId})">Shuffle Back In</button>
  </div>
  <CardRow
    v-if="showCards.ref.length > 0"
    :game="game"
    :playerId="playerId"
    :cards="showCards.ref"
    :isDiscards="viewingDiscard"
    :title="cardRowTitle"
    @choose="emit('choose', $event)"
    @close="hideCards"
  />
  <div class="deck-container">
    <div class="top-of-deck">
      <Treachery
        v-if="topOfDeckTreachery"
        :treachery="topOfDeckTreachery"
        :game="game"
        :data-index="topOfDeckTreachery.cardId"
        :playerId="playerId"
        class="deck"
        @choose="emit('choose', $event)"
      />
      <img
        v-else
        :class="{ 'deck--can-draw': drawCardsAction !== -1, 'card': topOfDeckRevealed }"
        class="deck"
        :src="topOfDeck"
        width="150px"
        @click="emit('choose', drawCardsAction)"
      />
      <span class="deck-size">{{investigator.deckSize}}</span>
      <button v-if="playTopOfDeckAction !== -1" @click="emit('choose', playTopOfDeckAction)">Play</button>
    </div>
    <template v-if="debug.active">
      <button @click="debug.send(game.id, {tag: 'Search', contents: ['Looking', investigatorId, {tag: 'GameSource', contents: []}, { tag: 'InvestigatorTarget', contents: investigatorId }, [[{tag: 'FromDeck', contents: []}, 'ShuffleBackIn']], {tag: 'BasicCardMatch', contents: {tag: 'AnyCard', contents: []}}, { tag: 'DrawFound', contents: [investigatorId, 1]}]})">Select Draw</button>
      <button @click="debug.send(game.id, {tag: 'ShuffleDeck', contents: {tag: 'InvestigatorDeck', contents: investigatorId}})">Shuffle</button>
    </template>
  </div>
</template>

<style scoped lang="scss">

.deck--can-draw {
  border: 2px solid var(--select);
  border-radius: 10px;
  cursor: pointer;
}

.discard {
  width: var(--card-width);
  button {
    white-space: nowrap;
    text-wrap: pretty;
  }

  &:deep(.card) {
    margin: 0;
    box-shadow: none;
  }

  &:deep(.card-container) {
    width: var(--card-width);
    margin: 0;
    position:relative;
    display: inline-flex;
    &::after {
      pointer-events: none;
      border-radius: 6px;
      content: "";
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background-color: #FFF;
      opacity: .85;
      mix-blend-mode: saturation;
    }
  }
  @media (max-width: 800px) and (orientation: portrait)  {
    :deep(button){
        display: none;
    }
    :deep(.card){
        max-width: calc(var(--pool-token-width)*1.2);
    }
  }
}

.deck, .card {
  border-radius: 6px;
  max-width: var(--card-width);
  @media (max-width: 800px) and (orientation: portrait)  {
    max-width: calc(var(--pool-token-width)*1.2);
  }
}

.view-discard-button {
  width: 100%;
}

.deck-container {
  display: flex;
  flex-direction: column;
  @media (max-width: 800px) and (orientation: portrait)  {
    margin-left: auto;
  }
}

.top-of-deck {
  position: relative;
  display: flex;
  flex-direction: column;
  width: fit-content;
}

.deck-size {
  background: rgba(0, 0, 0, 0.6);
  padding: 5px;
  border-radius: 20px;
  pointer-events: none;
  position: absolute;
  font-weight: bold;
  color: var(--title);
  inset: 0;
  width: fit-content;
  height: fit-content;
  aspect-ratio: 1;
  line-height: 1;
  margin: auto;
  transform: translateY(-28.0%);
}

.card-container {
  display: flex;
  flex-direction: column;
}

</style>
