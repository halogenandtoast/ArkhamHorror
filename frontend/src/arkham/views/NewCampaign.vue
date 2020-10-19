<template>
  <div v-if="ready">
    <div v-if="decks.length == 0">
      No decks, please add one first here <router-link to="/decks">here</router-link>
    </div>
    <form v-else id="new-campaign" @submit.prevent="start">
      <p>Number of Players</p>
      <input type="radio" v-model="playerCount" :value="1" /><label>1</label>
      <input type="radio" v-model="playerCount" :value="2" /><label>2</label>

      <p>Difficulty</p>
      <div v-for="difficulty in difficulties" :key="difficulty">
        <input
          type="radio"
          v-model="selectedDifficulty"
          :value="difficulty"
          :checked="difficulty === selectedDifficulty"
        />
        <label>{{difficulty}}</label>
      </div>

      <div>
        <p>Deck</p>
        <select v-model="deckId">
          <option disabled :value="null">-- Select a Deck--</option>
          <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
        </select>
      </div>

      <div>
        <input type="radio" v-model="standalone" :value="false"> <label>Campaign</label>
        <input type="radio" v-model="standalone" :value="true"> <label>Standalone</label>
      </div>

      <div v-if="standalone">
        <select v-model="selectedScenario">
          <option
            v-for="scenario in scenarios"
            :key="scenario.id"
            :value="scenario.id"
            :selected="scenario.id == selectedScenario"
            >{{scenario.name}}</option>
        </select>
      </div>
      <div v-else>
        <select v-model="selectedCampaign">
          <option
            v-for="campaign in campaigns"
            :key="campaign.id"
            :value="campaign.id"
            :selected="campaign.id == selectedCampaign"
            >{{campaign.name}}</option>
        </select>
      </div>

      <div>
        <p>Campaign Name</p>
        <input v-model="campaignName" :placeholder="currentCampaignName" />
      </div>

      <button type="submit" :disabled="disabled">Start</button>
    </form>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newGame } from '@/arkham/api';
import { Difficulty } from '@/arkham/types/Difficulty';

@Component
export default class NewCampaign extends Vue {
  private decks: Arkham.Deck[] = [];
  ready = false
  playerCount = 1
  difficulties: Difficulty[] = ['Easy', 'Standard', 'Hard', 'Expert'];
  selectedDifficulty: Difficulty = 'Easy'
  deckId: string | null = null
  standalone = false
  selectedCampaign = '01'
  selectedScenario = '81001'
  campaignName = null
  scenarios = [
    {
      id: '81001',
      name: 'Curse of the Rougarou',
    },
  ]
  campaigns = [
    {
      id: '01',
      name: 'Night of the Zealot',
    },
    {
      id: '02',
      name: 'The Dunwich Legacy',
    },
    {
      id: '03',
      name: 'The Path to Carcosa',
    },
    {
      id: '04',
      name: 'The Forgotten Age',
    },
  ]

  async mounted() {
    fetchDecks().then((decks) => {
      this.decks = decks;
      this.ready = true;
    });
  }

  get disabled() {
    return !this.deckId;
  }

  get currentCampaignName() {
    if (this.campaignName !== '' && this.campaignName !== null) {
      return this.campaignName;
    }

    return this.defaultCampaignName;
  }

  get defaultCampaignName() {
    const campaign = this.campaigns.find((c) => c.id === this.selectedCampaign);
    const scenario = this.scenarios.find((c) => c.id === this.selectedScenario);

    if (!this.standalone && campaign) {
      return `${campaign.name} - ${this.selectedDifficulty}`;
    }

    if (this.standalone && scenario) {
      return `${scenario.name} - ${this.selectedDifficulty}`;
    }

    return '';
  }

  start() {
    if (this.standalone) {
      const mscenario = this.scenarios.find((scenario) => scenario.id === this.selectedScenario);
      if (mscenario && this.deckId && this.currentCampaignName) {
        newGame(
          this.deckId,
          this.playerCount,
          null,
          mscenario.id,
          this.selectedDifficulty,
          this.currentCampaignName,
        ).then((game) => this.$router.push(`/games/${game.id}`));
      }
    } else {
      const mcampaign = this.campaigns.find((campaign) => campaign.id === this.selectedCampaign);
      if (mcampaign && this.deckId && this.currentCampaignName) {
        newGame(
          this.deckId,
          this.playerCount,
          mcampaign.id,
          null,
          this.selectedDifficulty,
          this.currentCampaignName,
        ).then((game) => this.$router.push(`/games/${game.id}`));
      }
    }
  }
}
</script>
