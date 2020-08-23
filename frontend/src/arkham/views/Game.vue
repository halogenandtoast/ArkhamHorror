<template>
  <div id="game" class="game" v-if="ready">
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
    <p v-if="game.currentData.gameOver">
      Game over
    </p>
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
  private socket: WebSocket | null = null;
  private game: Arkham.Game | null = null;
  private investigatorId: string | null = null;

  async mounted() {
    fetchGame(this.gameId).then(({ game, investigatorId }) => {
      this.game = game;
      this.investigatorId = investigatorId;
      const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
      this.socket = new WebSocket(`${baseURL}/api/v1/arkham/games/${this.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
      this.socket.addEventListener('message', (event) => {
        Arkham.gameDecoder.decodePromise(JSON.parse(event.data))
          .then((updatedGame) => { this.game = updatedGame; });
      });
      this.ready = true;
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

<style scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }

.game {
  width: 100vw;
  height: calc(100vh - 40px);
  display: grid;
  grid-template-rows: min-content min-content 1fr min-content;
}
</style>
