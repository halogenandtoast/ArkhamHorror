<template>
  <div id="new-game">
    <div>
      <select v-model="cycle">
        <option :key="option.id" v-for="option in cycles" :value="option">{{option.name}}</option>
      </select>
    </div>

    <div>
      <input type="text" v-model="deckUrl" placeholder="Deck url from arkhamdb.com" />
    </div>

    <div>
      <button :disabled="notReady" @click="startCampaign({ cycle, deckUrl })">Start!</button>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';
import { Cycle } from '@/arkham/types';

@Component
export default class NewCampaign extends Vue {
  private cycle: Cycle | null = null;
  private deckUrl = '';

  @Action fetchCycles!: () => Promise<void>
  @Action startCampaign!: (cycle: Cycle) => Promise<void>

  @Getter cycles!: Cycle[]

  async mounted() {
    await this.fetchCycles();
  }

  get notReady(): boolean {
    return this.cycle === null || this.deckUrl === '';
  }
}
</script>
