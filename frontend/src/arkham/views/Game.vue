<template>
  <div id="game" class="game" v-if="ready">
    <Scenario :game="game" @choose="choose" @update="update" />
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
  private game: Arkham.Game | null = null;

  async mounted() {
    fetchGame(this.gameId).then((game) => {
      this.game = game;
      this.ready = true;
    });
  }

  async choose(idx: number) {
    if (idx !== -1) {
      updateGame(this.gameId, idx).then((game) => {
        this.game = game;
      });
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
}
</style>
