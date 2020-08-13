<template>
  <div v-if="!game.currentData.gameOver" id="game" class="game">
    <CardOverlay />
    <StatusBar :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
    <PlayerOrder :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
    <div class="scenario-cards">
      <VictoryPile :game="game" />
      <div v-if="topOfEncounterDiscard" class="discard">
        <img
          :src="topOfEncounterDiscard"
          class="card"
        />
      </div>

      <EncounterDeck
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />

      <Agenda
        v-for="(agenda, key) in game.currentData.agendas"
        :key="key"
        :agenda="agenda"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />

      <Act
        v-for="(act, key) in game.currentData.acts"
        :key="key"
        :act="act"
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <img
        class="card"
        :src="'/img/arkham/cards/' + game.currentData.scenario.contents.id + '.jpg'"
      />
      <ChaosBag
        :game="game"
        :skillTest="game.currentData.skillTest"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <img
        v-if="activeCard"
        :src="activeCard"
        class="card"
      />
    </div>

    <div class="location-cards">
      <GameLog :game="game" />
      <div class="locations">
        <Location
          v-for="(location, key) in game.currentData.locations"
          class="location"
          :key="key"
          :game="game"
          :investigatorId="investigatorId"
          :location="location"
          @choose="$emit('choose', $event)"
        />
      </div>
    </div>

    <PlayerTabs :investigatorId="investigatorId">
      <Tab
        v-for="(player, index) in players"
        :key="index"
        :playerClass="player.contents.class"
        :title="player.contents.name"
        :investigatorId="index"
        :activePlayer="isActivePlayer(player)"
      >
        <Player
          :game="game"
          :investigatorId="investigatorId"
          :player="player"
          @choose="$emit('choose', $event)"
        />
      </Tab>
    </PlayerTabs>
    <ChoiceModal
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import { Investigator } from '@/arkham/types/Investigator';
import Player from '@/arkham/components/Player.vue';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import StatusBar from '@/arkham/components/StatusBar.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import ChoiceModal from '@/arkham/components/ChoiceModal.vue';
import PlayerTabs from '@/arkham/components/PlayerTabs.vue';
import PlayerOrder from '@/arkham/components/PlayerOrder.vue';
import Tab from '@/arkham/components/Tab.vue';
import EncounterDeck from '@/arkham/components/EncounterDeck.vue';
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import VictoryPile from '@/arkham/components/VictoryPile.vue';
import GameLog from '@/arkham/components/GameLog.vue';
import Location from '@/arkham/components/Location.vue';

@Component({
  components: {
    Player,
    Act,
    Agenda,
    Location,
    StatusBar,
    ChaosBag,
    ChoiceModal,
    PlayerTabs,
    Tab,
    EncounterDeck,
    PlayerOrder,
    CardOverlay,
    VictoryPile,
    GameLog,
  },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: Game;
  @Prop(String) readonly investigatorId!: string;

  private commitedCards: number[] = []
  private moving = false

  get activeCard() {
    if (this.game.currentData.activeCard) {
      const { cardCode } = this.game.currentData.activeCard.contents;
      return `/img/arkham/cards/${cardCode}.jpg`;
    }

    return null;
  }

  get players() {
    return this.game.currentData.investigators;
  }

  get topOfEncounterDiscard() {
    if (this.game.currentData.discard[0]) {
      const { cardCode } = this.game.currentData.discard[0];

      return `/img/arkham/cards/${cardCode}.jpg`;
    }

    return null;
  }

  isActivePlayer(player: Investigator) {
    return player.contents.id === this.game.currentData.activeInvestigatorId;
  }

  update(game: Game) {
    this.$emit('update', game);
  }
}

</script>

<style scoped lang="scss">
.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: 100px;
}

.card--sideways {
  width: auto;
  height: 200px;
}

.scenario-cards {
  display: flex;
  align-self: center;
  align-items: flex-start;
  justify-content: center;
  padding-bottom: 10px;
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
  justify-content: center;
  align-items: center;
  overflow: auto;
  min-height: 350px;
  position: relative;
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
  height: 100%;
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

.locations {
  box-sizing: border-box;
  flex: 1;
  display: flex;
  justify-content: center;
  align-self: center;
}
</style>
