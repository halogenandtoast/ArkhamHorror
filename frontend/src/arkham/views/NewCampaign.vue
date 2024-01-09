<script lang="ts" setup>
import { watch, ref, computed } from 'vue'
import { useUserStore } from '@/stores/user'
import type { User } from '@/types'
import { useRouter } from 'vue-router'
import * as Arkham from '@/arkham/types/Deck'
import { fetchDecks, newGame } from '@/arkham/api'
import { imgsrc } from '@/arkham/helpers'
import type { Difficulty } from '@/arkham/types/Difficulty'
import NewDeck from '@/arkham/components/NewDeck.vue'

import campaignJSON from '@/arkham/data/campaigns.json'
import scenarioJSON from '@/arkham/data/scenarios.json'
import sideStoriesJSON from '@/arkham/data/side-stories.json'

const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)

type GameMode = 'Campaign' | 'Standalone' | 'SideStory'

const gameMode = ref<GameMode>('Campaign')

const includeTarotReadings = ref(false)

const scenarios = computed(() => scenarioJSON.filter((s) =>
  s.beta
    ? currentUser.value && currentUser.value.beta
    : true
))

// const sideStories = computed(() => sideStoriesJSON.filter((s) =>
//   s.beta
//     ? currentUser.value && currentUser.value.beta
//     : true
// ))

const sideStories = computed(() => sideStoriesJSON)

const campaigns = computed(() => campaignJSON.filter((c) =>
  c.beta
    ? currentUser.value && currentUser.value.beta
    : true
))

const difficulties = computed<Difficulty[]>(() => {
  if(gameMode.value === 'SideStory') {
    const sideStoryScenario = sideStories.value.find((c) => c.id === selectedScenario.value)

    if (sideStoryScenario && sideStoryScenario.standaloneDifficulties) {
      return sideStoryScenario.standaloneDifficulties as Difficulty[]
    }

    return []
  }

  return ['Easy', 'Standard', 'Hard', 'Expert']
})

const router = useRouter()
const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const playerCount = ref(1)

type MultiplayerVariant = 'WithFriends' | 'TrueSolo'

const selectedDifficulty = ref<Difficulty>('Easy')
const deckIds = ref<(string | null)[]>([null, null, null, null])
const fullCampaign = ref(true)
const selectedCampaign = ref<string | null>(null)
const selectedScenario = ref<string | null>(null)
const campaignName = ref<string | null>(null)
const multiplayerVariant = ref<MultiplayerVariant>('WithFriends')
const returnTo = ref(false)

const campaignScenarios = computed(() => selectedCampaign.value
  ? scenarios.value.filter((s) => s.campaign == selectedCampaign.value)
  : []
)

const selectCampaign = (campaignId: string) => {
  selectedCampaign.value = campaignId,
  selectedScenario.value = null
}

watch(
  difficulties,
  async (newDifficulties) => selectedDifficulty.value = newDifficulties[0]
)

fetchDecks().then((result) => {
  decks.value = result;
  ready.value = true;
})

async function addDeck(d: Arkham.Deck) {
  decks.value = [d]
  deckIds.value[0] = d.id
}

const selectedCampaignReturnToId = computed(() => {
  const campaign = campaigns.value.find((c) => c.id === selectedCampaign.value);
  return campaign?.returnToId
})

const disabled = computed(() => {
  if (gameMode.value === 'Standalone' || gameMode.value === 'SideStory') {
    return !(scenario.value && currentCampaignName.value)
  } else {
    const mcampaign = campaigns.value.find((campaign) => campaign.id === selectedCampaign.value);
    return !(mcampaign && currentCampaignName.value)
  }
})

const defaultCampaignName = computed(() => {
  if (gameMode.value === 'Campaign' && campaign.value) {
    const returnToPrefix = returnTo.value ? "Return to " : ""
    return `${returnToPrefix}${campaign.value.name}`;
  }

  if (gameMode.value === 'Standalone' && scenario.value) {
    return `${scenario.value.name}`;
  }

  if (gameMode.value === 'SideStory' && scenario.value) {
    return `${scenario.value.name}`;
  }

  return '';
})

const currentCampaignName = computed(() => {
  return campaignName.value && campaignName.value !== ''
    ? campaignName.value
    : defaultCampaignName.value
})


const scenario = computed(() =>
  gameMode.value === 'SideStory'
    ? sideStories.value.find((s) => s.id === selectedScenario.value)
    : scenarios.value.find((s) => s.id === selectedScenario.value)
)

const campaign = computed(() =>
  gameMode.value === 'Campaign'
    ? campaigns.value.find((c) => c.id === selectedCampaign.value)
    : null
)

async function start() {
  if (gameMode.value === 'Standalone' || gameMode.value === 'SideStory') {
    if (scenario.value && currentCampaignName.value) {
      newGame(
        deckIds.value,
        playerCount.value,
        null,
        scenario.value.id,
        selectedDifficulty.value,
        currentCampaignName.value,
        multiplayerVariant.value,
        includeTarotReadings.value,
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
        fullCampaign.value ? null : selectedScenario.value,
        selectedDifficulty.value,
        currentCampaignName.value,
        multiplayerVariant.value,
        includeTarotReadings.value,
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

          <div class="options">
            <input type="radio" v-model="gameMode" :value="'Campaign'" id="campaign"> <label for="campaign">Campaign</label>
            <input type="radio" v-model="gameMode" :value="'Standalone'" id="standalone"> <label for="standalone">Standalone</label>
            <input type="radio" v-model="gameMode" :value="'SideStory'" id="sideStory"> <label for="sideStory">Side Story</label>
          </div>

          <template v-if="gameMode === 'SideStory'">
            <div class="scenarios">
              <div v-for="scenario in sideStories" :key="scenario.id">
                <img class="scenario-box" :class="{ 'selected-scenario': selectedScenario == scenario.id }" :src="imgsrc(`boxes/${scenario.id}.jpg`)" @click="selectedScenario = scenario.id">
              </div>
            </div>
          </template>
          <template v-else>
            <!-- <select v-model="selectedCampaign"> -->
              <div class="campaigns">
                <div v-for="campaign in campaigns" :key="campaign.id" class="campaign">
                  <img class="campaign-box" :class="{ 'selected-campaign': selectedCampaign == campaign.id }" :src="imgsrc(`boxes/${campaign.id}.jpg`)" @click="selectCampaign(campaign.id)">
                </div>
              </div>
            <!-- </select> -->
          </template>

          <div v-if="gameMode === 'Campaign' && selectedCampaign && selectedCampaignReturnToId" class="options">
            <input type="radio" v-model="returnTo" :value="false" id="normal"> <label for="normal">Normal</label>
            <input type="radio" v-model="returnTo" :value="true" id="returnTo"> <label for="returnTo">Return to...</label>
          </div>

          <div v-if="gameMode === 'Campaign' && campaign && campaign.settings" class="options">
            <input type="radio" v-model="fullCampaign" :value="true" id="full"> <label for="full">Full Campaign</label>
            <input type="radio" v-model="fullCampaign" :value="false" id="partial"> <label for="partial">Partial Campaign</label>
          </div>

          <template v-if="(gameMode === 'Standalone' || (gameMode !== 'SideStory' && !fullCampaign)) && selectedCampaign">
            <div class="scenarios">
              <div v-for="scenario in campaignScenarios" :key="scenario.id">
                <img class="scenario-box" :class="{ 'selected-scenario': selectedScenario == scenario.id }" :src="imgsrc(`boxes/${scenario.id}.jpg`)" @click="selectedScenario = scenario.id">
              </div>
            </div>
          </template>

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

          <p>Include Tarot Readings</p>
          <div class="options">
            <input
              type="radio"
              v-model="includeTarotReadings"
              :value="false"
              :checked="!includeTarotReadings"
              id="tarotNo"
            />
            <label for="tarotNo">No</label>
            <input
              type="radio"
              v-model="includeTarotReadings"
              :value="true"
              :checked="includeTarotReadings"
              id="tarotYes"
            />
            <label for="tarotYes">Yes</label>
          </div>

          <div>
            <p>Game Name</p>
            <input type="text" v-model="campaignName" :placeholder="currentCampaignName" />
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

input[type=checkbox] {
  display: none;
  /* margin: 10px; */
}

input[type=checkbox] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: desaturate(#6E8640, 30%);
  &:hover {
    background-color: desaturate(#6E8640, 20%);
  }

  &.invert {
    background: #6E8640;
    &:hover {
      background: #6E8640;
    }
  }
  border-color: #ddd;
}

input[type=checkbox]:checked + label {
  background: #6E8640;
  &.invert {
    background-color: desaturate(#6E8640, 30%);
  }
}

.invert[type=checkbox] + label {
    background: #6E8640;
    &:hover {
      background: #6E8640;
    }
}

.invert[type=checkbox]:checked + label {
  background-color: desaturate(#6E8640, 30%);
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
  gap: 10px;
  line-height: 0;

  grid-template-columns: repeat(auto-fill, calc((1 / 3 * 100%) - 7px));

  img {
    width: 100%;
  }
  margin-bottom: 10px;
}

.campaign-box:not(.selected-campaign) {
  -webkit-filter: grayscale(100%); /* Safari 6.0 - 9.0 */
  filter: grayscale(100%);
}

.scenarios {
  display: grid;
  line-height: 0;
  grid-template-columns: repeat(auto-fill, calc((1 / 4 * 100%) - 8px));
  gap: 10px;
  margin-bottom: 10px;

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
