<script lang="ts" setup>
import { computed, ref, inject, ComputedRef, reactive } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamCard from '@/arkham/types/Card';
import * as ArkhamGame from '@/arkham/types/Game';
import Enemy from '@/arkham/components/Enemy.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Card from '@/arkham/components/Card.vue';
import CardRow from '@/arkham/components/CardRow.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import ChoiceModal from '@/arkham/components/ChoiceModal.vue';
import * as Arkham from '@/arkham/types/Investigator';

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

export interface Props {
  game: Game
  player: Arkham.Investigator
  investigatorId: string
}

const props = defineProps<Props>()

const discards = computed<ArkhamCard.Card[]>(() => props.player.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))
const baseUrl = inject('baseUrl')

const topOfDiscard = computed(() => discards.value[0])

const topOfDeckRevealed = computed(() => props.player.modifiers?.some((m) => m.type.tag === "TopCardOfDeckIsRevealed"))

const topOfDeck = computed(() => {
  const topCard = props.player.deck[0]
  if  (topOfDeckRevealed.value && topCard) {
    return `${baseUrl}/img/arkham/cards/${topCard.cardCode.replace(/^c/, '')}.jpg`
  }
  return `${baseUrl}/img/arkham/player_back.jpg`
})

const playTopOfDeckAction = computed(() => {
  return choices.value.findIndex((c) => c.tag === "TargetLabel" && c.target.contents === props.player.deck[0].id)
})

const viewingDiscard = ref(false)
const viewDiscardLabel = computed(() => viewingDiscard.value ? "Close" : `${discards.value.length} Cards`)

const id = computed(() => props.player.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const drawCardsAction = computed(() => {
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


const doShowCards = (event: Event, cards: ComputedRef<ArkhamCard.Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}

const showDiscards = (e: Event) => doShowCards(e, discards, 'Discards', true)
const hideCards = () => showCards.ref = noCards

const debug = inject('debug')
const debugChoose = inject('debugChoose')
</script>

<template>
  <div class="player-cards">
    <section class="in-play">
      <Asset
        v-for="asset in player.assets"
        :asset="game.assets[asset]"
        :game="game"
        :investigatorId="investigatorId"
        :key="asset"
        @choose="$emit('choose', $event)"
        @showCards="doShowCards"
      />

      <Enemy
        v-for="enemyId in player.engagedEnemies"
        :key="enemyId"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />

      <Treachery
        v-for="treacheryId in player.treacheries"
        :key="treacheryId"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
    </section>

    <ChoiceModal
      :game="game"
      :investigatorId="id"
      @choose="$emit('choose', $event)"
    />

    <div class="player">
      <Investigator
        :player="player"
        :choices="choices"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
        @showCards="doShowCards"
      />

      <div class="discard">
        <Card v-if="topOfDiscard" :game="game" :card="topOfDiscard" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
        <button v-if="discards.length > 0" class="view-discard-button" @click="showDiscards">{{viewDiscardLabel}}</button>
      </div>

      <div class="deck-container">
        <div class="top-of-deck">
          <img
            :class="{ 'deck--can-draw': drawCardsAction !== -1, 'card': topOfDeckRevealed }"
            class="deck"
            :src="topOfDeck"
            width="150px"
            @click="$emit('choose', drawCardsAction)"
          />
          <span class="deck-size">{{player.deckSize}}</span>
          <button v-if="playTopOfDeckAction !== -1" @click="$emit('choose', playTopOfDeckAction)">Play</button>
        </div>
        <template v-if="debug">
          <button @click="debugChoose({tag: 'Search', contents: [investigatorId, {tag: 'GameSource', contents: []}, { tag: 'InvestigatorTarget', contents: investigatorId }, [[{tag: 'FromDeck', contents: []}, 'ShuffleBackIn']], {tag: 'AnyCard', contents: []}, { tag: 'DrawFound', contents: [investigatorId, 1]}]})">Select Draw</button>
        </template>
      </div>
      <section class="hand">
        <HandCard
          v-for="(card, index) in player.hand"
          :card="card"
          :game="game"
          :investigatorId="investigatorId"
          :key="index"
          @choose="$emit('choose', $event)"
        />

        <Treachery
          v-for="treacheryId in player.inHandTreacheries"
          :key="treacheryId"
          :treachery="game.treacheries[treacheryId]"
          :game="game"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />
      </section>
    </div>

    <CardRow
      v-if="showCards.ref.length > 0"
      :game="game"
      :investigatorId="investigatorId"
      :cards="showCards.ref"
      :isDiscards="viewingDiscard"
      :title="cardRowTitle"
      @choose="$emit('choose', $event)"
      @close="hideCards"
    />
  </div>
</template>

<style scoped lang="scss">
.player {
  display: flex;
  align-self: center;
  align-items: flex-start;
  padding: 10px;
  box-sizing: border-box;
}

.deck--can-draw {
  border: 3px solid $select;
  border-radius: 10px;
  cursor: pointer;
}

.discard {
  width: $card-width;
  margin-top: 10px;
  &:deep(.card) {
    margin: 0;
    box-shadow: none;
  }
  &:deep(.card-container) {
    width: $card-width;
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
      /* background-image: linear-gradient(120deg, #eaee44, #33d0ff); */
      opacity: .85;
      mix-blend-mode: saturation;
    }
  }
}

.deck, .card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  max-width: $card-width;
}

.deck {
  margin-top: 10px;
  width: auto;
}

.in-play {
  display: flex;
  background: #999;
  padding: 10px;
  box-sizing: border-box;

  @media (prefers-color-scheme: dark) {
    background: #1C1C1C;
  }

  & :deep(.asset) {
    margin-right: 5px;
  }
  & :deep(.enemy) {
    margin-right: 5px;
  }
}

.player-cards {
  box-sizing: border-box;
}

.hand {
  overflow-x: overlay;
  height: 100%;
  display: flex;
  padding-top: 10px;
}

.view-discard-button {
  width: 100%;
}

.deck-container {
  display: flex;
  flex-direction: column;
}

.top-of-deck {
  position: relative;
  display: flex;
  flex-direction: column;
}

.deck-size {
  pointer-events: none;
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 0.6);
  left: 50%;
  top: 43%;
  background: rgba(0, 0, 0, 0.6);
  padding: 10px;
  border-radius: 20px;
  transform: translateX(-50%) translateY(-50%);
}
</style>
