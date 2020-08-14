<template>
  <form id="new-campaign" @submit.prevent="start">
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

    <div v-for="n in playerCount" :key="n">
      <p>ArkhamDB deck url for player {{n}}</p>
      <input
        type="url"
        v-model="decks[n - 1]"
        @change="loadDeck(n - 1)"
        @paste.prevent="pasteDeck(n - 1, $event)"
      />
      <img v-if="investigators[n - 1]" :src="`/img/arkham/portraits/${investigators[n - 1]}.jpg`" />
    </div>

    <select v-model="selectedCampaign">
      <option
        v-for="campaign in campaigns"
        :key="campaign.id"
        :value="campaign.id"
        :selected="campaign.id == selectedCampaign"
        >{{campaign.name}}</option>
    </select>

    <button type="submit" :disabled="disabled">Start</button>
  </form>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { newGame } from '@/arkham/api';
import { Difficulty } from '@/arkham/types/Difficulty';

@Component
export default class NewCampaign extends Vue {
  playerCount = 1
  difficulties: Difficulty[] = ['Easy', 'Standard', 'Hard', 'Expert'];
  selectedDifficulty: Difficulty = 'Easy'
  decks: string[] = []
  investigators: string[] = []
  deckIds: string[] = []
  selectedCampaign = '01'
  campaigns = [
    {
      id: '01',
      name: 'Night of the Zealot',
      scenarioId: '01104',
    },
    {
      id: '02',
      name: 'The Dunwich Legacy',
      scenarioId: '01104',
    },
    {
      id: '03',
      name: 'The Path to Carcosa',
      scenarioId: '01104',
    },
    {
      id: '04',
      name: 'The Forgotten Age',
      scenarioId: '01104',
    },
  ]

  get disabled() {
    const currentInvestigators = this.investigators.slice(0, this.playerCount);

    return currentInvestigators.length !== this.playerCount || currentInvestigators.some((investigator) => investigator === '');
  }

  pasteDeck(idx: number, evt: ClipboardEvent) {
    if (evt.clipboardData) {
      this.$set(this.decks, idx, evt.clipboardData.getData('text'));
      this.loadDeck(idx);
    }
  }

  loadDeck(idx: number) {
    const matches = this.decks[idx].match(/\/decklist(\/view)?\/([^/]+)/);
    if (matches && matches[2]) {
      fetch(`https://arkhamdb.com/api/public/decklist/${matches[2]}`)
        .then((response) => response.json())
        .then((data) => {
          const deckId = matches[2];
          this.$set(this.investigators, idx, data.investigator_code);
          this.$set(this.deckIds, idx, deckId);
        });
    }
  }

  start() {
    const mcampaign = this.campaigns.find((campaign) => campaign.id === this.selectedCampaign);
    if (mcampaign) {
      newGame(this.deckIds, mcampaign.scenarioId, this.selectedDifficulty)
        .then((game) => this.$router.push(`/games/${game.id}`));
    }
  }
}
</script>
