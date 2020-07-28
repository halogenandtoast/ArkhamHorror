<template>
  <div>
    <section class="in-play">
      <Asset v-for="(asset, index) in player.assets" :asset="asset" :game="game" :key="index" />

      <Enemy
        v-for="enemyId in player.enemies"
        :key="enemyId"
        :enemyId="enemyId"
        :game="game"
        :focused="focusedEnemy === enemyId"
        @focusEnemy="focusedEnemy = $event"
        @update="$emit('update', $event)"
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
          width="200px"
        />
      </div>
      <img class="card" src="/img/arkham/player_back.jpg" width="150px" />
      <section class="hand">
        <HandCard
          v-for="(card, index) in player.contents.hand"
          :card="card"
          :key="index"
        />
      </section>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import Enemy from '@/arkham/components/Enemy.vue';
import Asset from '@/arkham/components/Asset.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import * as Arkham from '@/arkham/types/Investigator';

@Component({
  components: {
    Enemy,
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
    return this.player.contents.discard[0];
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
  max-width: 250px;
}

.in-play {
  display: flex;
}
</style>
