<script lang="ts" setup>
import { watch, ref, computed, inject } from 'vue';
import { useUserStore } from '@/stores/user';
import type { User } from '@/types';
import { useRouter } from 'vue-router';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newGame } from '@/arkham/api';
import type { Difficulty } from '@/arkham/types/Difficulty';
import NewDeck from '@/arkham/components/NewDeck';

import campaignJSON from '@/arkham/data/campaigns.json';
import scenarioJSON from '@/arkham/data/scenarios.json';
import sideStoriesJSON from '@/arkham/data/side-stories.json';

const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)
const baseUrl = inject('baseUrl')

const scenarios = computed(() => {
  return scenarioJSON.filter((s) => {
    if (s.beta) {
      return currentUser.value && currentUser.value.beta
    }
    return true
  })
})

const sideStories = computed(() => {
  return sideStoriesJSON.filter((s) => {
    if (s.beta) {
      return currentUser.value && currentUser.value.beta
    }
    return true
  })
})

const campaigns = computed(() => {
  return campaignJSON.filter((c) => {
    if (c.beta) {
      return currentUser.value && currentUser.value.beta
    }
    return true
  })
})

const difficulties: Difficulty[] = computed(() => {
  if(sideStory.value) {
    const sideStoryScenario = sideStories.value.find((c) => c.id === selectedScenario.value)
    if (sideStoryScenario.standaloneDifficulties) {
      return sideStoryScenario.standaloneDifficulties
    }
  }

  return['Easy', 'Standard', 'Hard', 'Expert']
})

const router = useRouter()
const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const playerCount = ref(1)

const selectedDifficulty = ref<Difficulty>('Easy')
const deckIds = ref<(string | null)[]>([null, null, null, null])
const standalone = ref(false)
const sideStory = ref(false)
const selectedCampaign = ref('01')
const selectedScenario = ref('81001')
const campaignName = ref<string | null>(null)
const multiplayerVariant = ref('WithFriends')
const returnTo = ref(false)

const campaignScenarios = computed(() => {
  if (selectedCampaign.value) {
    return scenarios.value.filter((s) => s.campaign == selectedCampaign.value)
  }

  return []
})

const selectCampaign = (campaignId) => {
  selectedCampaign.value = campaignId
  if(standalone.value && campaignScenarios.value.length == 0) {
    standalone.value = false
  }
}

watch(difficulties, async (newDifficulties) => selectedDifficulty.value = newDifficulties[0])

fetchDecks().then((result) => {
  decks.value = result;
  ready.value = true;
})

async function addDeck(d) {
  decks.value = [d]
  deckIds.value[0] = d.id
}

const selectedCampaignReturnToId = computed(() => {
  const campaign = campaigns.value.find((c) => c.id === selectedCampaign.value);
  if (campaign) {
    return campaign.returnToId;
  }

  return null;
})

const validStandalone = computed(() => {
  if (standalone.value) {
    return campaignScenarios.value.some((s) => s.id == selectedScenario.value)
  }

  return true
})

const disabled = computed(() => {
  if (multiplayerVariant.value == 'WithFriends') {
    return !deckIds.value[0] || !validStandalone.value
  } else {
    return [...Array(playerCount.value)].some((_,n) => !deckIds.value[n]) || !validStandalone.value
  }
})

const defaultCampaignName = computed(() => {
  const campaign = campaigns.value.find((c) => c.id === selectedCampaign.value);
  const scenario = sideStory.value
    ? sideStories.value.find((c) => c.id === selectedScenario.value)
    : scenarios.value.find((c) => c.id === selectedScenario.value)

  if (!standalone.value && !sideStory.value && campaign) {
    const returnToPrefix = returnTo.value ? "Return to " : ""
    return `${returnToPrefix}${campaign.name}`;
  }

  if (standalone.value && scenario) {
    return `${scenario.name}`;
  }

  if (sideStory.value && scenario) {
    return `${scenario.name}`;
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
  if (standalone.value || sideStory.value) {
    const mscenario = sideStory.value
      ? sideStories.value.find((scenario) => scenario.id === selectedScenario.value)
      : scenarios.value.find((scenario) => scenario.id === selectedScenario.value)
    if (mscenario && currentCampaignName.value) {
      newGame(
        deckIds.value,
        playerCount.value,
        null,
        mscenario.id,
        selectedDifficulty.value,
        currentCampaignName.value,
        multiplayerVariant.value,
      ).then((game) => router.push(`/games/${game.id}`));
    }
  } else {
    const mcampaign = campaigns.value.find((campaign) => campaign.id === selectedCampaign.value);
    if (mcampaign && currentCampaignName.value) {
      const campaignId = returnTo.value && mcampaign.returnToId ? mcampaign.returnToId : mcampaign.id
      newGame(
        deckIds.value,
        playerCount.value,
        campaignId,
        null,
        selectedDifficulty.value,
        currentCampaignName.value,
        multiplayerVariant.value,
      ).then((game) => router.push(`/games/${game.id}`));
    }
  }
}
</script>

<template>
  <div v-if="ready" class="container">
    <transition-group name="slide">
      <div v-if="decks.length == 0">
        <header>
          <h2>No decks, please add one first:</h2>
        </header>
        <NewDeck @new-deck="addDeck" />
      </div>
      <div v-else>
        <header>
          <h2>New Game</h2>
        </header>
        <form id="new-campaign" @submit.prevent="start">
          <div class="options">
            <input type="radio" v-model="sideStory" :value="false" id="campaign"> <label for="campaign">Campaign</label>
            <input type="radio" v-model="sideStory" :value="true" id="sideStory"> <label for="sideStory">Side Story</label>
          </div>

          <div v-if="sideStory">
            <div class="scenarios">
              <div v-for="scenario in sideStories" :key="scenario.id">
                <img class="scenario-box" :class="{ 'selected-scenario': selectedScenario == scenario.id }" :src="`${baseUrl}/img/arkham/boxes/${scenario.id}.jpg`" @click="selectedScenario = scenario.id">
              </div>
            </div>
          </div>
          <div v-else>
            <!-- <select v-model="selectedCampaign"> -->
              <div class="campaigns">
                <div v-for="campaign in campaigns" :key="campaign.id">
                  <img class="campaign-box" :class="{ 'selected-campaign': selectedCampaign == campaign.id }" :src="`${baseUrl}/img/arkham/boxes/${campaign.id}.jpg`" @click="selectCampaign(campaign.id)">
                </div>
              </div>
            <!-- </select> -->
          </div>

          <div v-if="!sideStory && selectedCampaign && selectedCampaignReturnToId" class="options">
            <input type="radio" v-model="returnTo" :value="false" id="normal"> <label for="normal">Normal</label>
            <input type="radio" v-model="returnTo" :value="true" id="returnTo"> <label for="returnTo">Return to...</label>
          </div>

          <div v-if="!sideStory && selectedCampaign && campaignScenarios.length > 0" class="options">
            <input type="radio" v-model="standalone" :value="false" id="fullCampaign"> <label for="fullCampaign">Full Campaign</label>
            <input type="radio" v-model="standalone" :value="true" id="standalone"> <label for="standalone">Standalone</label>
          </div>

          <div v-if="!sideStory && standalone && selectedCampaign">
            <div class="scenarios">
              <div v-for="scenario in campaignScenarios" :key="scenario.id">
                <img class="scenario-box" :class="{ 'selected-scenario': selectedScenario == scenario.id }" :src="`${baseUrl}/img/arkham/boxes/${scenario.id}.jpg`" @click="selectedScenario = scenario.id">
              </div>
            </div>
          </div>

          <p>Difficulty</p>
          <div class="options">
            <template v-for="difficulty in difficulties" :key="difficulty">
              <input
                type="radio"
                v-model="selectedDifficulty"
                :value="difficulty"
                :checked="difficulty === selectedDifficulty"
                :id="`difficulty${difficulty}`"
              />
              <label :for="`difficulty${difficulty}`">{{difficulty}}</label>
            </template>
          </div>

          <div>
            <p>Game Name</p>
            <input type="text" v-model="campaignName" :placeholder="currentCampaignName" />
          </div>

          <p>Number of Players</p>
          <div class="options">
            <input type="radio" v-model="playerCount" :value="1" id="player1" /><label for="player1">1</label>
            <input type="radio" v-model="playerCount" :value="2" id="player2" /><label for="player2">2</label>
            <input type="radio" v-model="playerCount" :value="3" id="player3" /><label for="player3">3</label>
            <input type="radio" v-model="playerCount" :value="4" id="player4" /><label for="player4">4</label>
          </div>
          <transition name="slide">
            <div v-if="playerCount > 1" class="options">
              <input type="radio" v-model="multiplayerVariant" value="WithFriends" id="friends" /><label for="friends">With Friends</label>
              <input type="radio" v-model="multiplayerVariant" value="Solo" id="solo" /><label for="solo">Multi-handed Solo</label>
            </div>
          </transition>


          <div v-if="playerCount == 1 || multiplayerVariant == 'WithFriends'">
            <p>Deck</p>
            <select v-model="deckIds[0]">
              <option disabled :value="null">-- Select a Deck--</option>
              <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
            </select>
          </div>
          <div v-else>

            <transition-group name="slide">
              <template v-for="idx in playerCount" :key="idx">
                <p>Deck {{idx}}</p>
                <select v-model="deckIds[idx - 1]">
                  <option disabled :value="null">-- Select a Deck--</option>
                  <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
                </select>
              </template>
            </transition-group>
          </div>


          <button type="submit" :disabled="disabled">Create</button>
        </form>
      </div>
    </transition-group>
  </div>
</template>

<style lang="scss" scoped>
.container {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
  margin-top: 10px;
}

#new-campaign {
  box-sizing: border-box;
  width: 100%;
  color: #FFF;
  background-color: #15192C;
  padding: 10px;
  border-radius: 3px;
  button {
    outline: 0;
    padding: 15px;
    background: #6E8640;
    text-transform: uppercase;
    color: white;
    border: 0;
    width: 100%;
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
  button[disabled] {
    background: #999;
    cursor: not-allowed;
    &:hover {
      background: #999;
    }
  }
  input[type=text] {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
  }
  select {
    -webkit-appearance: none;
    -moz-appearance: none;
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
    background-image:
      linear-gradient(45deg, transparent 50%, gray 50%),
      linear-gradient(135deg, gray 50%, transparent 50%),
      linear-gradient(to right, #ccc, #ccc);
    background-position:
      calc(100% - 25px) calc(1.3em + 2px),
      calc(100% - 20px) calc(1.3em + 2px),
      calc(100% - 3.5em) 0.5em;
    background-size:
      5px 5px,
      5px 5px,
      1px 2.5em;
    background-repeat: no-repeat;
  }
  a {
    color: #365488;
    font-weight: bolder;
  }
  p {
    margin: 0;
    padding: 0;
    text-transform: uppercase;
  }
}

h2 {
  color: #656A84;
  margin-left: 10px;
  text-transform: uppercase;
}

.difficulties {
  display: flex;
  flex-wrap: auto;
}

input[type=radio] {
  display: none;
  /* margin: 10px; */
}

input[type=radio] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: desaturate(#6E8640, 30%);
  &:hover {
    background-color: desaturate(#6E8640, 20%);
  }
  border-color: #ddd;
}

input[type=radio]:checked + label {
  background: #6E8640;
}

header {
  display: flex;
  align-items: center;
  justify-items: center;
  align-content: center;
  justify-content: center;
}

.back-link {
  font-size: 2em;
  color: #ff00ff;
  text-decoration: none;
}

.options {
  display: flex;
  margin-bottom: 10px;
  label {
    flex: 1;
    text-align: center;
    margin-left: 10px;
    &:nth-of-type(1) {
      margin-left: 0;
    }
  }
}

.campaigns {
  display: grid;

  grid-template-columns: repeat(auto-fill, calc(1 / 4 * 100%));

  img {
    width: 100%;
  }
}

.campaign-box:not(.selected-campaign) {
  -webkit-filter: grayscale(100%); /* Safari 6.0 - 9.0 */
  filter: grayscale(100%);
}

.scenarios {
  display: grid;
  grid-template-columns: repeat(auto-fill, calc(1 / 8 * 100%));

  img {
    width: 100%;
  }
}

.scenario-box:not(.selected-scenario) {
  -webkit-filter: grayscale(100%); /* Safari 6.0 - 9.0 */
  filter: grayscale(100%) sepia(0);
  transition: filter 1s linear;
  &:hover {
    filter: grayscale(100%) sepia(1);
    transition: filter 1s linear;
  }
}

.slide-enter-active,
.slide-leave-active {
  transition: all 0.3s ease-in-out;
}

.slide-enter-to,
.slide-leave-from {
  overflow: hidden;
  max-height: 1000px;
  opacity: 1;
}

.slide-enter-from,
.slide-leave-to {
  overflow: hidden;
  max-height: 0;
  opacity: 0;
}
</style>
