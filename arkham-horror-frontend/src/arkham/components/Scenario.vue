<template>
  <div id="game" class="game">
    <div class="scenario-cards">
      <div v-if="topOfEncounterDiscard" class="discard">
        <img
          :src="topOfEncounterDiscard"
          class="card"
          width="200px"
        />
      </div>
      <img class="card" src="/img/arkham/back.png" />

      <Agenda :agenda="game.gameState.stacks.Agenda" />
      <Act :act="game.gameState.stacks.Act" />
      <img class="card" :src="game.scenario.guide" />
      <ChaosBag
        :drawnToken="drawnToken"
        :canDrawToken="canDrawToken"
        :canApplyResult="canApplyResult"
        :skillDifficulty="skillDifficulty"
        :skillModifiedSkillValue="skillModifiedSkillValue"
        :pendingResult="pendingResult"
        @applyTokenResult="applyTokenResult"
      />

    </div>
    <div class="location-cards">
      <div
        v-for="location in game.gameState.locations"
        class="location"
        :key="location.name"
      >
        <img
          v-if="accessible(location)"
          @click="moveTo(location)"
          class="card location--can-move-to"
          :src="location.image"
        />
        <img
          v-else
          class="card"
          :src="location.image"
        />
        <div
          v-for="(uuid, index) in location.investigators"
          :key="index"
        >

          <img
            v-if="canMove(uuid) && !moving"
            @click="startMove(uuid)"
            :src="game.gameState.players[uuid].investigator.portrait"
            class="portrait portrait--can-move"
            width="80"
          />
          <img
            v-else
            :src="game.gameState.players[uuid].investigator.portrait"
            class="portrait"
            width="80"
          />
        </div>
        <div
          v-for="enemyId in location.enemies"
          :key="enemyId"
        >
          <img
            v-if="!game.gameState.enemies[enemyId].isEngaged"
            :src="game.gameState.enemies[enemyId].image"
            width="250"
          />
        </div>
        <div v-if="location.clues > 0" >
          <div
            v-if="canInvestigate"
            class="clue clue--can-investigate"
            @click="investigate(location)"
          >
            <img src="/img/arkham/clue.png" />
            {{location.clues}}
          </div>
          <div v-else>
            <img src="/img/arkham/clue.png" />
            {{location.clues}}
          </div>
        </div>
      </div>
    </div>
    <Player
      :game="game"
      :player="player"
      :commitedCards="commitedCards"
      @update="update"
      @commitCard="commitCard"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame, ArkhamStepTypes } from '@/arkham/types/game';
import { ArkhamLocation } from '@/arkham/types/location';
import { ArkhamAction, ArkhamActionTypes } from '@/arkham/types/action';
import {
  performAction,
  performDrawToken,
  performApplyTokenResult,
  performProgressAct,
} from '@/arkham/api';
import Player from '@/arkham/components/Player.vue';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';


@Component({
  components: {
    Player,
    Act,
    Agenda,
    ChaosBag,
  },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;

  private commitedCards: number[] = []
  private moving = false

  investigate(location: ArkhamLocation) {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.INVESTIGATE,
      contents: location.cardCode,
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.update(game);
    });
  }

  drawToken() {
    performDrawToken(this.game.id, this.commitedCards).then((game: ArkhamGame) => {
      this.update(game);
      this.commitedCards = [];
    });
  }

  applyTokenResult() {
    performApplyTokenResult(this.game.id).then((game: ArkhamGame) => {
      this.update(game);
    });
  }

  progressAct() {
    const { cardCode } = this.game.gameState.stacks.Act.contents[0];

    performProgressAct(this.game.id, cardCode).then((game: ArkhamGame) => {
      this.update(game);
    });
  }

  commitCard(cardIndex: number) {
    const index = this.commitedCards.indexOf(cardIndex);

    if (index === -1) {
      this.commitedCards.push(cardIndex);
    } else {
      this.commitedCards.splice(index, 1);
    }
  }

  update(game: ArkhamGame) {
    this.$emit('update', game);
  }

  canMove(uuid: string) {
    const { users, activeUser } = this.game.gameState;

    return users[activeUser] === uuid
      && this.player.actionsRemaining > 0
      && this.player.accessibleLocations.length > 0;
  }

  startMove(uuid: string) {
    if (this.canMove(uuid)) {
      this.moving = true;
    }
  }

  moveTo(location: ArkhamLocation) {
    const action: ArkhamAction = {
      tag: ArkhamActionTypes.MOVE,
      contents: location.cardCode,
    };

    performAction(this.game.id, action).then((game: ArkhamGame) => {
      this.moving = false;
      this.update(game);
    });
  }

  accessible(location: ArkhamLocation) {
    return this.accessibleLocations.includes(location.cardCode);
  }

  get accessibleLocations() {
    if (this.moving) {
      return this.player.accessibleLocations;
    }

    return [];
  }

  get player() {
    const { users, players, activeUser } = this.game.gameState;

    return players[users[activeUser]];
  }

  get drawnToken() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN) {
      return this.game.gameState.step.contents.token;
    }

    return null;
  }

  get canInvestigate() {
    const { step } = this.game.gameState;

    return step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION
      && this.player.actionsRemaining > 0;
  }

  get canDrawToken() {
    return this.game.gameState.step.tag === ArkhamStepTypes.SKILL_CHECK;
  }

  get canApplyResult() {
    return this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN;
  }

  get skillDifficulty() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN) {
      return this.game.gameState.step.contents.difficulty;
    }

    return 0;
  }

  get skillModifiedSkillValue() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN) {
      return this.game.gameState.step.contents.modifiedSkillValue;
    }

    return 0;
  }

  get pendingResult() {
    if (this.skillDifficulty > this.skillModifiedSkillValue) {
      return 'FAILURE';
    }

    return 'SUCCESS';
  }

  get topOfEncounterDiscard() {
    const mcard = this.game.gameState.encounterDiscard[0];
    if (mcard !== undefined && mcard !== null) {
      return mcard.image;
    }

    return null;
  }
}

</script>

<style scoped lang="scss">
.card {
  width: 250px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 13px;
  margin: 2px;
}

.card--sideways {
  width: auto;
  height: 250px;
}

.scenario-cards {
  display: flex;
  align-self: center;
  align-items: center;
}

.clue--can-investigate {
  border: 3px solid #ff00ff;
  border-radius: 100px;
  cursor: pointer;
}

.clue {
  position: relative;
  width: 57px;
  height: 54px;
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

.game {
  background-image: linear-gradient(darken(#E5EAEC, 10), #E5EAEC);
  width: 100%;
  z-index: 1;
}

.location-cards {
  display: flex;
}

.portrait {
  border-radius: 3px;
}

.portrait--can-move {
  cursor: pointer;
  border: 3px solid #FF00FF;
}

.location--can-move-to {
  border: 3px solid #FF00FF;
  cursor: pointer;
}

.agenda-container, .act-container {
  align-self: flex-start;
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
</style>
