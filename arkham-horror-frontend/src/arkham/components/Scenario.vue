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
      <Act :act="game.gameState.stacks.Act" @progressAct="progressAct" />
      <img class="card" :src="game.scenario.guide" />
      <ChaosBag
        :drawnToken="drawnToken"
        :canDrawToken="canDrawToken"
        :canApplyResult="canApplyResult"
        :skillDifficulty="skillDifficulty"
        :skillModifiedSkillValue="skillModifiedSkillValue"
        :pendingResult="pendingResult"
        @applyTokenResult="applyTokenResult"
        @drawToken="drawToken"
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
      :canTakeActions="canTakeActions"
      @update="update"
      @commitCard="commitCard"
    />
    <StatusBar :game="game" />
    <ChoiceModal v-if="shouldMakeChoice" :game="game" @choose="makeChoice" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import { Location } from '@/arkham/types/Location';
import Player from '@/arkham/components/Player.vue';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import StatusBar from '@/arkham/components/StatusBar.vue';
import ChoiceModal from '@/arkham/components/ChoiceModal.vue';

@Component({
  components: {
    Player,
    Act,
    Agenda,
    ChaosBag,
    StatusBar,
    ChoiceModal,
  },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: Game;

  private commitedCards: number[] = []
  private moving = false

  commitCard(cardIndex: number) {
    const index = this.commitedCards.indexOf(cardIndex);

    if (index === -1) {
      this.commitedCards.push(cardIndex);
    } else {
      this.commitedCards.splice(index, 1);
    }
  }

  update(game: Game) {
    this.$emit('update', game);
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
