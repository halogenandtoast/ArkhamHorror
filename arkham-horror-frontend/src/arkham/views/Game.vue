<template>
  <div id="game" v-if="ready">
    <Scenario :game="game" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { ArkhamGame } from '@/arkham/types/ArkhamGame';
import { fetchGame } from '@/api';
import Scenario from '@/arkham/components/Scenario.vue';

@Component({
  components: { Scenario },
})
export default class Game extends Vue {
  @Prop(String) readonly gameId!: string;

  private ready = false;
  private game: ArkhamGame | null = null;

  async mounted() {
    fetchGame(this.gameId).then((game) => {
      this.game = game;
      this.ready = true;
    });
  }
}
</script>

<style scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }
</style>
