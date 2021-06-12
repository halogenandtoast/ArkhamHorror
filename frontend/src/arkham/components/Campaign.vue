<template>

  <div v-if="upgradeDeck" id="game" class="game">
    <UpgradeDeck :game="game" :investigatorId="investigatorId" />
  </div>
  <div v-else-if="game.currentData.gameState === 'IsActive'" id="game" class="game">
    <Scenario
      v-if="game.currentData.scenario"

      :game="game"
      :gameLog="gameLog"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
      @update="update"
    />
    <template v-else>
      <CardOverlay />
      <StatusBar :game="game" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
      <PlayerOrder
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
      <PlayerSelector
        :game="game"
        :investigatorId="investigatorId"
        @choose="$emit('choose', $event)"
      />
    </template>
  </div>
  <div v-else-if="game.currentData.gameState === 'IsPending'">
    <div id='invite'>
      <h3>Waiting for more players</h3>
      <div v-if="investigatorId == game.currentData.leadInvestigatorId">
        <p>Invite them with this url: {{inviteLink}}</p>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import StatusBar from '@/arkham/components/StatusBar.vue';
import PlayerOrder from '@/arkham/components/PlayerOrder.vue';
import Scenario from '@/arkham/components/Scenario.vue';
import UpgradeDeck from '@/arkham/components/UpgradeDeck.vue';
import PlayerSelector from '@/arkham/components/PlayerSelector.vue';
import CardOverlay from '@/arkham/components/CardOverlay.vue';

export default defineComponent({
  components: {
    StatusBar,
    PlayerOrder,
    PlayerSelector,
    CardOverlay,
    Scenario,
    UpgradeDeck,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    gameLog: { type: Array, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props, { emit }) {
    const inviteLink = `${window.location.href}/join` // fix-syntax`

    async function update(game: Game) {
      emit('update', game);
    }

    const upgradeDeck = computed(() => props.game.currentData.campaign && props.game.currentData.campaign.contents.step.tag === 'UpgradeDeckStep')

    return { inviteLink, update, upgradeDeck }
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

.game {
  background-image: linear-gradient(darken(#E5EAEC, 10), #E5EAEC);
  width: 100%;
  z-index: 1;
}

#invite {
  background: #FFF;
  padding: 5px 10px;
  width: 800px;
  margin: 0 auto;
  margin-top: 50px;
  border-radius: 5px;
  text-align: center;
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
