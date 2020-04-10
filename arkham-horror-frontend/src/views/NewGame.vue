<template>
  <div id="new-game">
    <select v-model="cycle">
      <option :key="option" v-for="option in cycles">{{option}}</option>
    </select>

    <select v-model="scenario">
      <option :key="option" v-for="option in scenarios">{{option}}</option>
    </select>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';

@Component
export default class NewGame extends Vue {
  private cycle: string | null = null;
  private scenario: string | null = null;

  @Action fetchCycles!: () => Promise<void>
  @Action fetchScenarios!: () => Promise<void>

  @Getter cycles!: string[]
  @Getter cycleScenarios!: (cycle: string) => string[]

  async mounted() {
    await this.fetchCycles();
    await this.fetchScenarios();
  }

  get scenarios() {
    if (this.cycle) {
      return this.cycleScenarios(this.cycle);
    }

    return [];
  }
}
</script>
