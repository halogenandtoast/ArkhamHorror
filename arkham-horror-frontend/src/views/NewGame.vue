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
import { ArkhamHorrorCycle, ArkhamHorrorScenario } from '@/arkham/types';

@Component
export default class NewGame extends Vue {
  private cycle: ArkhamHorrorCycle | null = null;
  private scenario: ArkhamHorrorScenario | null = null;

  @Action fetchCycles!: () => Promise<void>
  @Action fetchScenarios!: () => Promise<void>
  @Action startGame!: (cycle: ArkhamHorrorCycle, secenario: ArkhamHorrorScenario) => Promise<void>

  @Getter cycles!: ArkhamHorrorCycle[]
  @Getter cycleScenarios!: (cycle: ArkhamHorrorCycle) => ArkhamHorrorScenario[]

  async mounted() {
    await this.fetchCycles();
    await this.fetchScenarios();
  }

  get notReady(): boolean {
    return this.cycle === null && this.scenario === null;
  }

  get scenarios(): ArkhamHorrorScenario[] {
    if (this.cycle) {
      return this.cycleScenarios(this.cycle);
    }

    return [];
  }
}
</script>
