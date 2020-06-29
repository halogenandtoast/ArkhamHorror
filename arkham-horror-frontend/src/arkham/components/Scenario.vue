<template>
  <div id="game" class="game">
    <div class="scenario-cards">
      <img class="card" :src="game.scenario.guide" />
      <img
        class="card card--sideways"
        :src="game.gameState.stacks.Agenda.contents[0].image"
      />
      <div v-if="game.gameState.stacks.Agenda.contents[0].doom">
        <img src="/img/arkham/doom.png"/> {{game.gameState.stacks.Agenda.contents[0].doom}}
      </div>
      <img
        class="card card--sideways"
        :src="game.gameState.stacks.Act.contents[0].image"
      />

      <img v-if="drawnToken" :src="chaosTokenSrc" class="token" />
      <img
        v-else-if="canDrawToken"
        class="token token--can-draw"
        src="/img/arkham/ct_+1.png"
        @click="drawToken"
      />
      <img v-else class="token" src="/img/arkham/ct_+1.png" />
      <div v-if="canApplyResult">
        <p>
          Difficulty: {{skillDifficulty}},
          Modified Skill: {{skillModifiedSkillValue}},
          Pending Result: {{pendingResult}}
        </p>
        <button @click="applyTokenResult">Apply Result</button>
      </div>
    </div>
    <div class="location-cards">
      <div
        v-for="location in game.gameState.locations"
        class="location"
        :key="location.name"
      >
        <img
          class="card"
          :src="location.image"
        />
        <div
          v-for="(investigator, index) in location.investigators"
          :key="index"
        >
          <img
            :src="investigator.portrait"
            width="80"
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
      :player="game.gameState.player"
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
import { performAction, performDrawToken, performApplyTokenResult } from '@/arkham/api';
import Player from '@/arkham/components/Player.vue';


@Component({
  components: { Player },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: ArkhamGame;

  private commitedCards: number[] = []

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

  get drawnToken() {
    if (this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN) {
      return this.game.gameState.step.contents.token;
    }

    return null;
  }

  get canInvestigate() {
    return this.game.gameState.step.tag === ArkhamStepTypes.INVESTIGATOR_ACTION
      && this.game.gameState.player.actionsRemaining > 0;
  }

  get canDrawToken() {
    return this.game.gameState.step.tag === ArkhamStepTypes.SKILL_CHECK;
  }

  get canApplyResult() {
    return this.game.gameState.step.tag === ArkhamStepTypes.REVEAL_TOKEN;
  }

  get chaosTokenSrc() {
    return `/img/arkham/ct_${this.drawnToken}.png`;
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

.token--can-draw {
  border: 5px solid #ff00ff;
  border-radius: 500px;
}

.token {
  width: 150px;
  height: auto;
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
</style>
