<template>
  <div id="game" v-if="ready">
    <div v-if="socketError" class="socketWarning">
       <p>Your game is out of sync, trying to reconnect...</p>
    </div>
    <div class="game">
      <Campaign
        v-if="game.currentData.campaign"
        :game="game"
        :investigatorId="investigatorId"
        @choose="choose"
        @update="update"
      />
      <Scenario
        v-else-if="!game.currentData.gameOver"
        :game="game"
        :investigatorId="investigatorId"
        @choose="choose"
        @update="update"
      />
      <div v-if="game.currentData.gameOver">
        <p>Game over</p>

        <div v-for="entry in game.currentData.campaign.contents.log.recorded" :key="entry">
          {{entry}}
        </div>

        <div v-for="(entry, idx) in game.currentData.campaign.contents.log.recordedSets" :key="idx">
          {{entry[0]}}: {{entry[1].join(", ")}}
        </div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue'
import * as Arkham from '@/arkham/types/Game'
import { fetchGame, updateGame } from '@/arkham/api'
import Scenario from '@/arkham/components/Scenario.vue'
import Campaign from '@/arkham/components/Campaign.vue'

export default defineComponent({
  components: { Scenario, Campaign },
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const ready = ref(false)
    const socketError = ref(false)
    const socket = ref<WebSocket | null>(null)
    const game = ref<Arkham.Game | null>(null)
    const investigatorId = ref<string | null>(null)

    function connect() {
      const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
      socket.value = new WebSocket(`${baseURL}/api/v1/arkham/games/${props.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
      socket.value.addEventListener('open', () => {
        ready.value = true;
        socketError.value = false;
      });
      socket.value.addEventListener('message', (event: MessageEvent) => {
        Arkham.gameDecoder.decodePromise(JSON.parse(event.data))
          .then((updatedGame) => { game.value = updatedGame; });
      });
      socket.value.addEventListener('error', () => {
        socketError.value = true;
        if (socket.value) {
          socket.value.close();
        }
      });
      socket.value.addEventListener('close', () => {
        socketError.value = true;
        setTimeout(() => connect(), 1000);
      });
    }

    fetchGame(props.gameId).then(({ game: newGame, investigatorId: newInvestigatorId }) => {
      game.value = newGame;
      investigatorId.value = newInvestigatorId;
      connect();
    });

    async function choose(idx: number) {
      if (idx !== -1 && game.value && game.value.currentData) {
        updateGame(props.gameId, idx, game.value.currentData.hash);
      }
    }

    async function update(state: Arkham.Game) {
      game.value = state;
    }

    return { socketError, ready, game, investigatorId, choose, update }
  }
})
</script>

<style lang="scss" scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }

.game {
  width: 100vw;
  height: calc(100vh - 40px);
}

.socketWarning  {
  backdrop-filter: blur(3px);
  background-color: rgba(0,0,0,0.8);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  display: flex;
  z-index: 100;

  justify-content: center;
  align-items: center;
  justify-self: center;
  align-self: center;

  p {
    padding: 10px;
    background: #FFF;
    border-radius: 4px;
  }
}
</style>
