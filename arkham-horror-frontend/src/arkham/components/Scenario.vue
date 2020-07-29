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

      <Agenda
        v-for="(agenda, key) in game.currentData.agendas"
        :key="key"
        :agenda="agenda"
      />
      <Act
        v-for="(act, key) in game.currentData.acts"
        :key="key"
        :act="act"
      />
      <img
        class="card"
        :src="'/img/arkham/cards/' + game.currentData.scenario.contents.id + '.jpg'"
      />
      <ChaosBag
        :game="game"
        @choose="$emit('choose', $event)"
      />
    </div>
    <div class="location-cards">
      <Location
        v-for="(location, key) in game.currentData.locations"
        class="location"
        :key="key"
        :game="game"
        :location="location"
        @choose="$emit('choose', $event)"
      />
    </div>
    <Player
      :game="game"
      :player="player"
      @choose="$emit('choose', $event)"
    />
    <StatusBar :game="game" @choose="$emit('choose', $event)" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { Game } from '@/arkham/types/Game';
import Player from '@/arkham/components/Player.vue';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import StatusBar from '@/arkham/components/StatusBar.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import Location from '@/arkham/components/Location.vue';

@Component({
  components: {
    Player,
    Act,
    Agenda,
    Location,
    StatusBar,
    ChaosBag,
  },
})
export default class Scenario extends Vue {
  @Prop(Object) readonly game!: Game;

  private commitedCards: number[] = []
  private moving = false

  get player() {
    return Object.values(this.game.currentData.investigators)[0];
  }

  get topOfEncounterDiscard() {
    return this.game.currentData.discard[0];
  }

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
