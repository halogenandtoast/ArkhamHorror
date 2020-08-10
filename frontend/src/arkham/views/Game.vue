<template>
  <div id="game" class="game" v-if="ready">
    <Scenario v-if="!game.currentData.gameOver" :game="game" @choose="choose" @update="update" />
    <p v-if="game.currentData.gameOver">
      Game over
    </p>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Game';
import { fetchGame, updateGame } from '@/arkham/api';
import api from '@/api';
import Scenario from '@/arkham/components/Scenario.vue';

@Component({
  components: { Scenario },
})
export default class Game extends Vue {
  @Prop(String) readonly gameId!: string;

  private ready = false;
  private socket: WebSocket | null = null;
  private game: Arkham.Game | null = null;

  async mounted() {
    fetchGame(this.gameId).then((game) => {
      this.game = game;
      this.socket = new WebSocket(`${api.defaults.baseURL}/arkham/games/${this.gameId}`.replace(/https?/, 'ws'));
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
