<template>
  <div v-if="!game.currentData.gameOver" id="scenario" class="scenario">
    <CardOverlay />
    <StatusBar :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
    <PlayerOrder :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
    <PlayerSelector
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />
    <div class="scenario-cards">
      <div v-if="topEnemyInVoid">
        <Enemy
          :enemy="topEnemyInVoid"
          :game="game"
          :investigatorId="investigatorId"
          @choose="$emit('choose', $event)"
        />
      </div>
      <img
        v-if="scenarioDeck"
        :src="scenarioDeck"
        class="card"
      />
      <VictoryDisplay :game="game" />
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
        :src="scenarioGuide"
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

    <div class="location-cards" :style="locationStyles">
      <Location
        v-for="(location, key) in game.currentData.locations"
        class="location"
        :key="key"
        :game="game"
        :investigatorId="investigatorId"
        :location="location"
        :style="{ 'grid-area': location.contents.label, 'justify-self': 'center' }"
        @choose="$emit('choose', $event)"
      />
    </div>

    <PlayerTabs
      :game="game"
      :investigatorId="investigatorId"
      :players="players"
      :activePlayerId="activePlayerId"
      @choose="$emit('choose', $event)"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import Act from '@/arkham/components/Act.vue';
import Agenda from '@/arkham/components/Agenda.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import StatusBar from '@/arkham/components/StatusBar.vue';
import ChaosBag from '@/arkham/components/ChaosBag.vue';
import PlayerTabs from '@/arkham/components/PlayerTabs.vue';
import PlayerOrder from '@/arkham/components/PlayerOrder.vue';
import PlayerSelector from '@/arkham/components/PlayerSelector.vue';
import EncounterDeck from '@/arkham/components/EncounterDeck.vue';
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import VictoryDisplay from '@/arkham/components/VictoryDisplay.vue';
import Location from '@/arkham/components/Location.vue';

export default defineComponent({
  components: {
    Act,
    Agenda,
    Location,
    StatusBar,
    ChaosBag,
    PlayerTabs,
    EncounterDeck,
    PlayerOrder,
    PlayerSelector,
    CardOverlay,
    VictoryDisplay,
    Enemy,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props, { emit }) {
    const scenarioGuide = computed(() => {
      const { scenario } = props.game.currentData;
      if (!scenario) {
        return '';
      }

      const { id, difficulty } = scenario.contents;
      const difficultySuffix = difficulty === 'Hard' || difficulty === 'Expert'
        ? 'b'
        : '';

      return `/img/arkham/cards/${id}${difficultySuffix}.jpg`;
    })

    const scenarioDeck = computed(() => {
      const { scenario } = props.game.currentData;
      if (!scenario || !scenario.contents.deck) {
        return null;
      }

      const { tag } = scenario.contents.deck;

      switch(tag) {
        case 'ExhibitDeck':
          return `/img/arkham/cards/02132b.jpg`;
        default:
          return null;
      }
    })

    const locationStyles = computed(() => {
      const { scenario } = props.game.currentData;
      if (!scenario) {
        return null;
      }
      const { locationLayout } = scenario.contents;
      if (locationLayout) {
        return {
          display: 'grid',
          'grid-template-areas': locationLayout.map((row) => `"${row}"`).join(' '),
          'grid-row-gap': '10px',
        };
      }
      return null;
    })

    const activeCard = computed(() => {
      if (props.game.currentData.activeCard) {
        const { cardCode } = props.game.currentData.activeCard.contents;
        return `/img/arkham/cards/${cardCode}.jpg`;
      }

      return null;
    })

    const players = computed(() => props.game.currentData.investigators)

    const topOfEncounterDiscard = computed(() => {
      if (props.game.currentData.discard[0]) {
        const { cardCode } = props.game.currentData.discard[0];

        return `/img/arkham/cards/${cardCode}.jpg`;
      }

      return null;
    })

    const topEnemyInVoid = computed(() => Object.values(props.game.currentData.enemiesInVoid)[0])
    const activePlayerId = computed(() => props.game.currentData.activeInvestigatorId)

    function update(game: Game) {
      emit('update', game);
    }

    return {
      update,
      activePlayerId,
      topOfEncounterDiscard,
      players,
      activeCard,
      locationStyles,
      scenarioGuide,
      scenarioDeck,
      topEnemyInVoid
    }
  }
})
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

.scenario {
  background-image: linear-gradient(darken(#E5EAEC, 10), #E5EAEC);
  width: 100%;
  height: 100%;
  z-index: 1;
  display: grid;
  grid-template-rows: min-content min-content 1fr min-content;
}

.location-cards {
  display: flex;
  justify-content: center;
  align-items: center;
  overflow: auto;
  min-height: 350px;
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
</style>
