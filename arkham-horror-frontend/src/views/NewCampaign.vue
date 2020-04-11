<template>
  <div id="new-game">
    <select v-model="cycle">
      <option :key="option.id" v-for="option in cycles" :value="option">{{option.name}}</option>
    </select>

    <button :disabled="notReady" @click="startCampaign({ cycle })">Start!</button>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';
import { Cycle } from '@/arkham/types';

@Component
export default class NewCampaign extends Vue {
  private cycle: Cycle | null = null;

  @Action fetchCycles!: () => Promise<void>
  @Action startCampaign!: (cycle: Cycle) => Promise<void>

  @Getter cycles!: Cycle[]

  async mounted() {
    await this.fetchCycles();
  }

  get notReady(): boolean {
    return this.cycle === null;
  }
}
</script>
