<template>
  <div class="player-cards">
    <section class="in-play">
      <Asset
        v-for="asset in player.contents.assets"
        :asset="game.currentData.assets[asset]"
        :game="game"
        :key="asset"
        @choose="$emit('choose', $event)"
      />

      <Enemy
        v-for="enemyId in player.contents.engagedEnemies"
        :key="enemyId"
        :enemy="game.currentData.enemies[enemyId]"
        :game="game"
        @choose="$emit('choose', $event)"
      />

      <Treachery
        v-for="treacheryId in player.contents.treacheries"
        :key="treacheryId"
        :treachery="game.currentData.treacheries[treacheryId]"
        :game="game"
        @choose="$emit('choose', $event)"
      />
    </section>
    <div class="player">
      <Investigator
        :player="player"
        :game="game"
        @choose="$emit('choose', $event)"
      />
      <div v-if="topOfDiscard" class="discard">
        <img
          :src="topOfDiscard"
          class="card"
          width="150px"
        />
      </div>
      <img
        :class="{ 'deck--can-draw': drawCardsAction !== -1 }"
        class="card"
        src="/img/arkham/player_back.jpg"
        width="150px"
        @click="$emit('choose', drawCardsAction)"
      />
      <section class="hand">
        <HandCard
          v-for="(card, index) in player.contents.hand"
          :card="card"
          :game="game"
          :key="index"
          @choose="$emit('choose', $event)"
        />
      </section>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import Enemy from '@/arkham/components/Enemy.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import Asset from '@/arkham/components/Asset.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import * as Arkham from '@/arkham/types/Investigator';

@Component({
  components: {
    Enemy,
    Treachery,
    Asset,
    HandCard,
    Investigator,
  },
})
export default class Player extends Vue {
  @Prop(Object) readonly game!: Game
  @Prop(Object) readonly player!: Arkham.Investigator
  @Prop(Array) readonly commitedCards!: number[]
  @Prop(Boolean) readonly canTakeActions!: boolean

  private focusedEnemy: string | null = null;

  get topOfDiscard() {
    if (this.player.contents.discard[0]) {
      const { cardCode } = this.player.contents.discard[0];
      return `/img/arkham/cards/${cardCode}.jpg`;
    }

    return null;
  }

  get choices() {
    return choices(this.game);
  }

  get drawCardsAction() {
    return this.choices.findIndex((choice) => choice.tag === MessageType.DRAW_CARDS);
  }
}
</script>

<style scoped lang="scss">
.hand {
  display: flex;
}

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
  position: relative;
  &::after {
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

.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 13px;
  margin: 2px;
  max-width: 130px;
}

.in-play {
  display: flex;
  background: #999;
  padding: 10px;
  box-sizing: border-box;
}

/deep/ .in-play .card {
  width: 130px;
}

.player-cards {
  width: 100vw;
  box-sizing: border-box;
}

.hand {
  overflow-x: scroll;
  height: 100%;
}
</style>
