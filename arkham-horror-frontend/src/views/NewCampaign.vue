<template>
  <div id="new-game">
    <div>
      <select v-model="cycle">
        <option :key="option.id" v-for="option in cycles" :value="option">{{option.name}}</option>
      </select>
    </div>

    <div>
      <select v-model="difficulty">
        <option
          :key="option"
          v-for="option in difficulties"
          :value="option"
        >{{option}}</option>
      </select>
    </div>

    <div>
      <input type="text" v-model="deckUrl" placeholder="Deck url from arkhamdb.com" />
    </div>

    <div>
      <button
        :disabled="notReady"
        @click="start"
      >Start!</button>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { Action, Getter } from 'vuex-class';
import {
  ArkhamCycle,
  ArkhamDifficulty,
  ArkhamGame,
  ArkhamSettings,
} from '@/arkham/types';

@Component
export default class NewCampaign extends Vue {
  private cycle: ArkhamCycle | null = null;
  private deckUrl = '';
  private difficulty: ArkhamDifficulty | null = null;
  private difficulties: ArkhamDifficulty[] = ['Easy', 'Standard', 'Hard', 'Expert']

  @Action fetchCycles!: () => Promise<void>
  @Action startCampaign!: (campaignSettings: ArkhamSettings) => Promise<ArkhamGame>

  @Getter cycles!: ArkhamCycle[]

  async mounted() {
    await this.fetchCycles();
  }

  get notReady(): boolean {
    return this.cycle === null || this.deckUrl === '' || this.difficulty === null;
  }

  start() {
    if (this.cycle && this.difficulty && this.deckUrl) {
      this.startCampaign({
        cycle: this.cycle,
        difficulty: this.difficulty,
        deckUrl: this.deckUrl,
      }).then((campaign) => {
        this.$router.push({ path: `/campaigns/${campaign}` });
      });
    }
  }
}
</script>
