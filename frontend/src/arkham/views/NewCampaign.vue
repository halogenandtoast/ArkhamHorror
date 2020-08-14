<template>
  <div id="new-campaign">
    <p>Number of Players</p>
    <input type="radio" v-model="playerCount" :value="1" /><label>1</label>
    <input type="radio" v-model="playerCount" :value="2" /><label>2</label>

    <div v-for="n in playerCount" :key="n">
      <p>ArkhamDB deck url for player {{n}}</p>
      <input type="url" v-model="decks[n]" @change="loadDeck(n)" />
    </div>

    <select v-model="selectedCampaign">
      <option
        v-for="campaign in campaigns"
        :key="campaign.id"
        :value="campaign.id"
        :selected="campaign.id == selectedCampaign"
        >{{campaign.name}}</option>
    </select>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';

@Component
export default class NewCampaign extends Vue {
  playerCount = 1
  decks: string[] = []
  investigators: string[] = []
  deckIds: string[] = []
  selectedCampaign = '01'
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

  loadDeck(idx: number) {
    const matches = this.decks[idx].match(/\/decklist(\/view)?\/([^/]+)/);
    if (matches && matches[2]) {
      fetch(`https://arkhamdb.com/api/public/decklist/${matches[2]}`)
        .then((response) => response.json())
        .then((data) => console.log(data)) // eslint-disable-line
    }
  }
}
</script>
