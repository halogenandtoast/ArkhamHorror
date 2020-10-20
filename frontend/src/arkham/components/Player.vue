<template>
  <div class="player-cards">
    <section class="in-play">
      <Asset
        v-for="asset in player.contents.assets"
        :asset="game.currentData.assets[asset]"
        :game="game"
        :investigatorId="investigatorId"
        :key="asset"
        @choose="$emit('choose', $event)"
      />

      <Enemy
        v-for="enemyId in player.contents.engagedEnemies"
        :key="enemyId"
        :enemy="game.currentData.enemies[enemyId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />

      <Treachery
        v-for="treacheryId in player.contents.treacheries"
        :key="treacheryId"
        :treachery="game.currentData.treacheries[treacheryId]"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
    </section>
    <div class="player">
      <Investigator
        :player="player"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <div class="discard">
        <img
          v-if="topOfDiscard"
          :src="topOfDiscard"
          class="card"
        />
      </div>
      <img
        :class="{ 'deck--can-draw': drawCardsAction !== -1 }"
        class="deck"
        src="/img/arkham/player_back.jpg"
        width="150px"
        @click="$emit('choose', drawCardsAction)"
      />
      <section class="hand">
        <HandCard
          v-for="(card, index) in player.contents.hand"
          :card="card"
          :game="game"
          :investigatorId="investigatorId"
          :key="index"
          @choose="$emit('choose', $event)"
        />
      </section>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import Enemy from '@/arkham/components/Enemy.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import * as Arkham from '@/arkham/types/Investigator';

export default defineComponent({
  components: {
    Enemy,
    Treachery,
    Asset,
    HandCard,
    Investigator,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    player: { type: Object as () => Arkham.Investigator, required: true },
    investigatorId: { type: String, required: true },
    canTakeActions: { type: Boolean, required: true },
  },
  setup(props) {
    const topOfDiscard = computed(() => {
      if (props.player.contents.discard[0]) {
        const { cardCode } = props.player.contents.discard[0];
        return `/img/arkham/cards/${cardCode}.jpg`;
      }

      return null;
    })

    const id = computed(() => props.player.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    const drawCardsAction = computed(() => {
      return choices
        .value
        .findIndex((c) => c.tag === MessageType.DRAW_CARDS && c.contents[0] === id.value);
    })

    return { topOfDiscard, drawCardsAction }
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
  border: 3px solid #FF00FF;
  border-radius: 10px;
  cursor: pointer;
}

.discard {
  width: 110px;
  margin-top: 10px;
  position: relative;
  &::after {
    pointer-events: none;
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

.deck, .card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  max-width: 100px;
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
}

/deep/ .in-play .card {
  width: 130px;
  margin: 0 2px;
}

.player-cards {
  width: 100vw;
  box-sizing: border-box;
}

.hand {
  overflow-x: scroll;
  height: 100%;
  display: flex;
  padding-top: 10px;
}
</style>
