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

    <div>
      <p>ArkhamDB deck url</p>
      <input
        type="url"
        v-model="deck"
        @change="loadDeck"
        @paste.prevent="pasteDeck($event)"
      />
      <img v-if="investigator" :src="`/img/arkham/portraits/${investigator}.jpg`" />
    </div>

    <div>
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
  deck: string | null = null
  investigator: string | null = null
  deckId: string | null = null
  selectedCampaign = '01'
  campaignName = null
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

  get disabled() {
    return !this.investigator;
  }

  get currentCampaignName() {
    if (this.campaignName !== '' && this.campaignName !== null) {
      return this.campaignName;
    }

    return this.defaultCampaignName;
  }

  get defaultCampaignName() {
    const campaign = this.campaigns.find((c) => c.id === this.selectedCampaign);

    if (campaign) {
      return `${campaign.name} - ${this.selectedDifficulty}`;
    }

    return '';
  }

  pasteDeck(evt: ClipboardEvent) {
    if (evt.clipboardData) {
      this.deck = evt.clipboardData.getData('text');
      this.loadDeck();
    }
  }

  loadDeck() {
    if (!this.deck) {
      return;
    }

    const matches = this.deck.match(/\/decklist(\/view)?\/([^/]+)/);
    if (matches && matches[2]) {
      fetch(`https://arkhamdb.com/api/public/decklist/${matches[2]}`)
        .then((response) => response.json())
        .then((data) => {
          const deckId = matches[2];
          this.investigator = data.investigator_code;
          this.deckId = deckId;
        });
    }
  }

  start() {
    const mcampaign = this.campaigns.find((campaign) => campaign.id === this.selectedCampaign);
    if (mcampaign && this.deckId && this.currentCampaignName) {
      newGame(
        this.deckId,
        this.playerCount,
        mcampaign.id,
        this.selectedDifficulty,
        this.currentCampaignName,
      ).then((game) => this.$router.push(`/games/${game.id}`));
    }
  }
}
</script>
