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
import { defineComponent, ref, computed } from 'vue';
import { useRouter } from 'vue-router';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newGame } from '@/arkham/api';
import { Difficulty } from '@/arkham/types/Difficulty';

const scenarios = [
  {
    id: '81001',
    name: 'Curse of the Rougarou',
  },
]

const campaigns = [
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

const difficulties: Difficulty[] = ['Easy', 'Standard', 'Hard', 'Expert']

export default defineComponent({
  setup() {
    const router = useRouter()
    const decks = ref<Arkham.Deck[]>([])
    const ready = ref(false)
    const playerCount = ref(1)

    const selectedDifficulty = ref<Difficulty>('Easy')
    const deckId = ref<string | null>(null)
    const standalone = ref(false)
    const selectedCampaign = ref('01')
    const selectedScenario = ref('81001')
    const campaignName = ref<string | null>(null)

    fetchDecks().then((result) => {
      decks.value = result;
      ready.value = true;
    })

    const disabled = computed(() => !deckId.value)
    const defaultCampaignName = computed(() => {
      const campaign = campaigns.find((c) => c.id === selectedCampaign.value);
      const scenario = scenarios.find((c) => c.id === selectedScenario.value);

      if (!standalone.value && campaign) {
        return `${campaign.name} - ${selectedDifficulty.value}`;
      }

      if (standalone.value && scenario) {
        return `${scenario.name} - ${selectedDifficulty.value}`;
      }

      return '';
    })

    const currentCampaignName = computed(() => {
      if (campaignName.value !== '' && campaignName.value !== null) {
        return campaignName.value;
      }

      return defaultCampaignName.value;
    })

    async function start() {
      if (standalone.value) {
        const mscenario = scenarios.find((scenario) => scenario.id === selectedScenario.value);
        if (mscenario && deckId.value && currentCampaignName.value) {
          newGame(
            deckId.value,
            playerCount.value,
            null,
            mscenario.id,
            selectedDifficulty.value,
            currentCampaignName.value,
          ).then((game) => router.push(`/games/${game.id}`));
        }
      } else {
        const mcampaign = campaigns.find((campaign) => campaign.id === selectedCampaign.value);
        if (mcampaign && deckId.value && currentCampaignName.value) {
          newGame(
            deckId.value,
            playerCount.value,
            mcampaign.id,
            null,
            selectedDifficulty.value,
            currentCampaignName.value,
          ).then((game) => router.push(`/games/${game.id}`));
        }
      }
    }

    return {
      ready,
      start,
      standalone,
      disabled,
      campaignName,
      currentCampaignName,
      difficulties,
      scenarios,
      campaigns,
      decks,
      deckId,
      playerCount,
      selectedDifficulty,
      selectedScenario,
      selectedCampaign
    }
  }
})
</script>
