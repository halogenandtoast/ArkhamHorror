<template>
  <div>
    <section class="in-play">
      <div v-for="(card, index) in player.inPlay" :key="index">
        <img :src="card.contents.image" class="card" />
        <div
          v-if="card.contents.uses && card.contents.uses > 0"
          class="poolItem poolItem-resource"
        >
          <img src="/img/arkham/resource.png" />
          {{card.contents.uses}}
        </div>
      </div>

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
        width="200px"
      />
      <img v-else class="card" src="/img/arkham/player_back.jpg" width="200px" />
      <div>
        <img class="card" :src="player.investigator.image" />
        <p><i class="action" v-for="n in player.actionsRemaining" :key="n"></i></p>
        <button @click="endTurn">End turn</button>
      </div>
      <div>
        <div v-if="canTakeResources" class="poolItem poolItem-resource" @click="takeResource">
          <img
            class="resource--can-take"
            src="/img/arkham/resource.png"
          />
          {{player.resources}}
        </div>
        <div v-else class="poolItem poolItem-resource">
          <img src="/img/arkham/resource.png" />
          {{player.resources}}
        </div>
        <div class="poolItem"><img src="/img/arkham/clue.png"/> {{player.clues}}</div>
        <div class="poolItem"><img src="/img/arkham/health.png"/> {{player.healthDamage}}</div>
        <div class="poolItem"><img src="/img/arkham/sanity.png"/> {{player.sanityDamage}}</div>
      </div>
      <section class="hand">
        <div v-for="(card, index) in player.hand" :key="index">
          <img
            v-if="canPlay(index)"
            class="card playable"
            :src="card.contents.image"
            @click="playCard(index)"
          />
          <img
            v-else-if="canCommit(index)"
            :class="['card', 'commitable', { commited: isCommited(index) }]"
            :src="card.contents.image"
            @click="commitCard(index)"
          />
          <img v-else class="card" :src="card.contents.image" />
        </div>

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

@Component({
  components: { Enemy },
})
export default class Player extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
  @Prop(Object) readonly player!: ArkhamPlayer
  @Prop(Array) readonly commitedCards!: number[]

  private focusedEnemy: string | null = null;

  commitCard(cardIndex: number) {
    this.$emit('commitCard', cardIndex);
  }

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
  .card {
    width: 150px;
    border-radius: 7px;
  }

  .playable {
    border: 2px solid #ff00ff;
    cursor: pointer;
  }

  .commitable {
    border: 2px solid #ff00ff;
  }
}

.player {
  display: flex;
  align-self: center;
  align-items: flex-start;
}

.poolItem {
  position: relative;
  width: 57px;
  height: 73px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.5em;

  img {
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
    z-index: -1;
  }
}

.poolItem-resource {
  padding-right:5px;
  clip-path: polygon(50% 0%, 100% 25%, 100% 75%, 50% 100%, 0% 75%, 0% 25%);
}

.resource--can-take {
  padding: 3px;
  cursor: pointer;
  background-color: #FF00FF;
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

  &.commited {
    margin-top: -10px;
  }
}

i.action {
  font-family: 'Arkham';
  speak: none;
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;

  &:before {
    font-family: "Arkham";
    content: "\0049";
  }
}

.in-play {
  display: flex;
}
</style>
