<template>
  <div class="player-cards">
    <section class="in-play">
      <Asset
        v-for="asset in player.contents.assets"
        :asset="game.assets[asset]"
        :game="game"
        :investigatorId="investigatorId"
        :key="asset"
        @choose="$emit('choose', $event)"
      />

      <Enemy
        v-for="enemyId in player.contents.engagedEnemies"
        :key="enemyId"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />

      <Treachery
        v-for="treacheryId in player.contents.treacheries"
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
        :game="game"
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
            :class="{ 'deck--can-draw': drawCardsAction !== -1 }"
            class="deck"
            :src="`${baseUrl}/img/arkham/player_back.jpg`"
            width="150px"
            @click="$emit('choose', drawCardsAction)"
          />
          <span class="deck-size">{{player.deckSize}}</span>
        </div>
        <template v-if="debug">
          <button @click="debugChoose({tag: 'Search', contents: [investigatorId, {tag: 'GameSource', contents: []}, { tag: 'InvestigatorTarget', contents: investigatorId }, [[{tag: 'FromDeck', contents: []}, 'ShuffleBackIn']], [], { tag: 'DrawFound', contents: [investigatorId, 1]}]})">Select Draw</button>
        </template>
      </div>
      <section class="hand">
        <HandCard
          v-for="(card, index) in player.contents.hand"
          :card="card"
          :game="game"
          :investigatorId="investigatorId"
          :key="index"
          @choose="$emit('choose', $event)"
        />

        <Treachery
          v-for="treacheryId in player.contents.inHandTreacheries"
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

<script lang="ts">
import { defineComponent, computed, ref, inject, ComputedRef, reactive } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamCard from '@/arkham/types/Card';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
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

export default defineComponent({
  components: {
    Enemy,
    Treachery,
    Asset,
    HandCard,
    Investigator,
    ChoiceModal,
    CardRow,
    Card,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    player: { type: Object as () => Arkham.Investigator, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {

    const discards = computed<ArkhamCard.Card[]>(() => props.player.contents.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';

    const topOfDiscard = computed(() => discards.value[0])


    const viewingDiscard = ref(false)
    const viewDiscardLabel = computed(() => viewingDiscard.value ? "Close" : `${discards.value.length} Cards`)

    const id = computed(() => props.player.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const drawCardsAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.DRAW_CARDS && c.contents[0] === id.value);
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

    return { id, cardRowTitle, debug, debugChoose, doShowCards, showCards, baseUrl, discards, topOfDiscard, drawCardsAction, hideCards, showDiscards, viewingDiscard, viewDiscardLabel }
  }
})
</script>

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
}

.deck-size {
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 0.6);
  left: 50%;
  top: 43%;
  transform: translateX(-50%) translateY(-50%);
}
</style>
