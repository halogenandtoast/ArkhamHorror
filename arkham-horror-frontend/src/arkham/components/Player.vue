<template>
  <div>
    <section class="in-play">
      <PlayerCard v-for="(card, index) in player.inPlay" :card="card" :game="game" :key="index" />

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
        :canTakeResources="canTakeResources"
        @endTurn="endTurn"
        @takeResource="takeResource"
      />
      <div v-if="topOfDiscard" class="discard">
        <img
          :src="topOfDiscard"
          class="card"
          width="200px"
        />
      </div>
      <img
        v-if="canDraw"
        class="card deck--can-draw"
        @click="drawCard"
        src="/img/arkham/player_back.jpg"
        width="150px"
      />
      <img v-else class="card" src="/img/arkham/player_back.jpg" width="150px" />
      <section class="hand">
        <HandCard
          v-for="(card, index) in player.hand"
          :card="card"
          :canPlay="canPlay(index)"
          :canCommit="canCommit()"
          :isCommited="isCommited(index)"
          :key="index"
          @playCard="$emit('playCard', index)"
          @commitCard="$emit('commitCard', index)"
        />
      </section>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { ArkhamPlayer } from '@/arkham/types';
import { ArkhamAction, ArkhamActionTypes } from '@/arkham/types/action';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';
import { performAction, performEndTurn } from '@/arkham/api';
import Enemy from '@/arkham/components/Enemy.vue';
import PlayerCard from '@/arkham/components/PlayerCard.vue';
import HandCard from '@/arkham/components/HandCard.vue';
import Investigator from '@/arkham/components/Investigator.vue';

@Component({
  components: {
    Enemy,
    PlayerCard,
    HandCard,
    Investigator,
  },
})
export default class Player extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
  @Prop(Object) readonly player!: ArkhamPlayer
  @Prop(Array) readonly commitedCards!: number[]

  private focusedEnemy: string | null = null;

  playCard(index: number) {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.PLAY_CARD,
      contents: index,
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.$emit('update', game);
    });
  }

  canPlay(index: number) {
    if (!this.actionWindow || this.focusedEnemy) {
      return false;
    }


    const card = this.player.hand[index];

    if (this.player.actionsRemaining === 0 && card.tag === 'PlayerCard' && !card.contents.isFast) {
      return false;
    }

    const mcost = 'cost' in card.contents ? card.contents.cost : 0;

    if (mcost === null || mcost === undefined) {
      return false;
    }

    return mcost <= this.player.resources;
  }

  canCommit() {
    return this.commitWindow;
  }

  isCommited(cardIndex: number) {
    return this.commitedCards.indexOf(cardIndex) !== -1;
  }

  get canDraw() {
    if (this.focusedEnemy) {
      return false;
    }

    return this.actionWindow && this.player.actionsRemaining > 0;
  }

  get canTakeResources() {
    if (this.focusedEnemy) {
      return false;
    }

    return this.actionWindow && this.player.actionsRemaining > 0;
  }

  get actionWindow() {
    return this.game.gameState.step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION;
  }

  get commitWindow() {
    return this.game.gameState.step.tag === ArkhamStepTypes.SKILL_CHECK;
  }

  get topOfDiscard() {
    const mcard = this.player.discard[0];
    if (mcard !== undefined && mcard !== null) {
      return mcard.contents.image;
    }

    return null;
  }

  endTurn() {
    if (this.player.actionsRemaining > 0) {
      if (!window.confirm('You still have actions remaining. Continue?')) { // eslint-disable-line
        return;
      }
    }

    performEndTurn(this.game.id).then((game: ArkhamGame) => {
      this.$emit('update', game);
    });
  }

  takeResource() {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.TAKE_RESOURCE,
      contents: [],
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.$emit('update', game);
    });
  }

  drawCard() {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.DRAW_CARD,
      contents: [],
    };
    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.$emit('update', game);
    });
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
