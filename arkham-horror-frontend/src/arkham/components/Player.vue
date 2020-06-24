<template>
  <div>
    <div class="in-play">
      <section>
        <h2>In play</h2>
        <div v-for="(card, index) in player.inPlay" :key="index">
          <img :src="card.contents.image" />
          <div
            v-if="card.contents.uses && card.contents.uses > 0"
            class="poolItem poolItem-resource"
          >
            <img src="/img/arkham/resource.png" />
            {{card.contents.uses}}
          </div>
        </div>
      </section>
    </div>
    <div class="player">
      <img
        v-if="topOfDiscard"
        :src="topOfDiscard"
        width="200px"
      />
      <img
        v-if="canDraw"
        class="deck--can-draw"
        @click="drawCard"
        src="/img/arkham/player_back.jpg"
        width="200px"
      />
      <img v-else src="/img/arkham/player_back.jpg" width="200px" />
      <img :src="player.investigator.image" />
      <div>
        <div v-if="actionWindow" class="poolItem poolItem-resource" @click="takeResource">
          <img
            class="resource--can-take"
            src="/img/arkham/resource.png"
          />
          {{player.resources}}
        </div>
        <div v-else>
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
import { performAction } from '@/arkham/api';

@Component
export default class Player extends Vue {
  @Prop(Object) readonly game!: ArkhamGame
  @Prop(Object) readonly player!: ArkhamPlayer

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
    if (!this.actionWindow) {
      return false;
    }

    const card = this.player.hand[index];
    const mcost = 'cost' in card.contents ? card.contents.cost : 0;

    if (mcost === null || mcost === undefined) {
      return false;
    }

    return mcost <= this.player.resources;
  }

  get canDraw() {
    return this.actionWindow;
  }

  get actionWindow() {
    return this.game.gameState.step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION;
  }

  get topOfDiscard() {
    const mcard = this.player.discard[0];
    if (mcard !== undefined && mcard !== null) {
      return mcard.contents.image;
    }

    return null;
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
  }
}

.player {
  display: flex;
  align-self: center;
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
}
</style>
