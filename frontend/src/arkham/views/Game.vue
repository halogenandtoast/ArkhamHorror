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
import { Component, Prop, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Game';
import { fetchGame, updateGame } from '@/arkham/api';
import Scenario from '@/arkham/components/Scenario.vue';
import Campaign from '@/arkham/components/Campaign.vue';

@Component({
  components: { Scenario, Campaign },
})
export default class Game extends Vue {
  @Prop(String) readonly gameId!: string;

  private ready = false;
  private socketError = false;
  private socket: WebSocket | null = null;
  private game: Arkham.Game | null = null;
  private investigatorId: string | null = null;

  async mounted() {
    fetchGame(this.gameId).then(({ game, investigatorId }) => {
      this.game = game;
      this.investigatorId = investigatorId;
      this.connect();
    });
  }

  connect() {
    const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
    this.socket = new WebSocket(`${baseURL}/api/v1/arkham/games/${this.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
    this.socket.addEventListener('open', () => {
      this.ready = true;
      this.socketError = false;
    });
    this.socket.addEventListener('message', (event) => {
      Arkham.gameDecoder.decodePromise(JSON.parse(event.data))
        .then((updatedGame) => { this.game = updatedGame; });
    });
    this.socket.addEventListener('error', () => {
      this.socketError = true;
      setTimeout(() => this.connect(), 5000);
    });
    this.socket.addEventListener('close', () => {
      this.socketError = true;
      setTimeout(() => this.connect(), 5000);
    });
  }

  async choose(idx: number) {
    if (idx !== -1 && this.game && this.game.currentData) {
      updateGame(this.gameId, idx, this.game.currentData.hash);
    }
  }

  update(state: Arkham.Game) {
    this.game = state;
  }
}
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
