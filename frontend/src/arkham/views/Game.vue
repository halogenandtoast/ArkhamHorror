<template>
  <div id="game" class="game" v-if="ready">
    <Scenario
      v-if="!game.currentData.gameOver"
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

@Component({
  components: { Scenario },
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
      this.socket = new WebSocket(`${window.location.protocol}//${window.location.hostname}/api/v1/arkham/games/${this.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
      this.socket.addEventListener('message', (event) => {
        Arkham.gameDecoder.decodePromise(JSON.parse(event.data))
          .then((updatedGame) => { this.game = updatedGame; });
      });
      this.ready = true;
    });
  }

  async choose(idx: number) {
    if (idx !== -1) {
      updateGame(this.gameId, idx);
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
  grid-template-rows: min-content 1fr min-content;
}
</style>
