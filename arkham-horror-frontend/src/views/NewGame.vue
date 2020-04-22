<template>
  <div id="new-game">
    <select v-model="cycle">
      <option :key="option.id" v-for="option in cycles" :value="option">{{option.name}}</option>
    </select>

    <select v-model="scenario">
      <option
        v-for="option in scenarios"
        :key="option.id"
        :value="option"
      >{{option.name}}</option>
    </select>

    <button :disabled="notReady" @click="startGame({cycle, scenario})">Start!</button>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';
import { ArkhamCycle, ArkhamScenario } from '@/arkham/types';

@Component
export default class NewGame extends Vue {
  private cycle: ArkhamCycle | null = null;
  private scenario: ArkhamScenario | null = null;

  @Action fetchCycles!: () => Promise<void>
  @Action fetchScenarios!: () => Promise<void>
  @Action startGame!: (cycle: ArkhamCycle, secenario: ArkhamScenario) => Promise<void>

  @Getter cycles!: ArkhamCycle[]
  @Getter cycleScenarios!: (cycle: ArkhamCycle) => ArkhamScenario[]

  async mounted() {
    await this.fetchCycles();
    await this.fetchScenarios();
  }

  get notReady(): boolean {
    return this.cycle === null && this.scenario === null;
  }

  get scenarios(): ArkhamScenario[] {
    if (this.cycle) {
      return this.cycleScenarios(this.cycle);
    }

    return [];
  }
}
</script>
