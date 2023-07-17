<script lang="ts" setup>
import { watch, ref, computed } from 'vue';
import { useUserStore } from '@/stores/user';
import type { User } from '@/types';
import { useRouter } from 'vue-router';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newGame } from '@/arkham/api';
import { imgsrc } from '@/arkham/helpers';
import type { Difficulty } from '@/arkham/types/Difficulty';
import type { StandaloneSetting } from '@/types/StandaloneSetting'
import { CampaignLogSettings } from '@/arkham/types/CampaignSettings'
import NewDeck from '@/arkham/components/NewDeck';
import { toCapitalizedWords } from '@/arkham/helpers';

import campaignJSON from '@/arkham/data/campaigns.json';
import scenarioJSON from '@/arkham/data/scenarios.json';
import sideStoriesJSON from '@/arkham/data/side-stories.json';

const store = useUserStore()
const currentUser = computed<User | null>(() => store.getCurrentUser)

type GameMode = 'Campaign' | 'Standalone' | 'SideStory'

const gameMode = ref<GameMode>('Campaign')
const campaignLog = ref<CampaignLogSettings>({ keys: [], counts: {}, sets: {}, options: [] })

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
  if(gameMode.value === 'SideStory') {
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
const fullCampaign = ref(true)
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

const selectCampaign = (campaignId) => { selectedCampaign.value = campaignId }

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
  if (gameMode.value === 'Standalone') {
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

  if (gameMode.value === 'Campaign') {
    const returnToPrefix = returnTo.value ? "Return to " : ""
    return `${returnToPrefix}${campaign.name}`;
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
  if (campaignName.value !== '' && campaignName.value !== null) {
    return campaignName.value;
  }

  return defaultCampaignName.value;
})


const scenario = computed(() => {
    return gameMode.value === 'SideStory'
      ? sideStories.value.find((s) => s.id === selectedScenario.value)
      : scenarios.value.find((s) => s.id === selectedScenario.value)
})

const campaign = computed(() => {
  if (gameMode.value === 'Campaign') {
    return campaigns.value.find((c) => c.id === selectedCampaign.value)
  }

  return null
})

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
        standaloneSettings.value,
        campaignLog.value
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
        standaloneSettings.value,
        campaignLog.value
      ).then((game) => router.push(`/games/${game.id}`));
    }
  }
}

const standaloneSettings = ref<StandaloneSetting[]>([])

// computed standaloneSettings is a bit of a hack, because nested values change by value
// when we change standaloneSettings they are "cached" so to avoid this we deep copy the
// standaloneSettings in order to never alter its original value.
const computedStandaloneSettings = computed<StandaloneSetting[]>(() => {
  if (gameMode.value === 'Standalone') {
    return JSON.parse(JSON.stringify(scenario.value?.settings || []))
  }

  return []
})

watch(computedStandaloneSettings, (newSettings) => {
  standaloneSettings.value = newSettings
}, { immediate: true })

type SettingCondition =
  { type: "key", key: string } |
  { type: "inSet", key: string, recordable: string, content: Recordable }

type Recordable = { key: string, content: string }

type CampaignSetting =
  { type: "CrossOut", key: string, ckey: string, recordable: string, content: Recordable, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseNum", key: string, ckey: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseKey", key: string, content: string[], ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ForceKey", key: string, content: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[]} |
  { type: "SetKey", key: string, ckey: string, ifRecorded?: SettingCondition[] } |
  { type: "Option", key: string, ckey: string, ifRecorded?: SettingCondition[] } |
  { type: "SetRecordable", key: string, recordable: string, content: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[] } |
  { type: "ChooseRecordable", key: string, ckey: string, recordable: string, content: Recordable[], ifRecorded?: SettingCondition[] }

type CampaignScenario = { key: string, ifRecorded?: SettingCondition[], anyRecorded?: SettingCondition[], settings: CampaignSetting[] }

const campaignSettings = ref<CampaignScenario[]>([])

// computed standaloneSettings is a bit of a hack, because nested values change by value
// when we change standaloneSettings they are "cached" so to avoid this we deep copy the
// standaloneSettings in order to never alter its original value.
const computedCampaignSettings = computed<CampaignScenario[]>(() => {
  selectedScenario.value
  return JSON.parse(JSON.stringify(campaign.value?.settings || []))
})

watch(computedCampaignSettings, (newSettings) => {
  if (selectedScenario.value) {
    const idx = newSettings.findIndex((s) => s.scenarioId === selectedScenario.value)
    if (idx !== -1) {
      const relevantSettings = newSettings.slice(0, idx)

      const crossOut = relevantSettings.flatMap((s) => s.settings.filter(s => s.type === "CrossOut"))
      const sets = crossOut.reduce((a, s) => {
        return { ...a, [s.key]: { recordable: s.recordable, entries: s.content.map((c) => { return { tag: "Recorded", value: c.content }}) } }}, {})

      campaignLog.value = { keys: [], counts: {}, options: [], sets }

      campaignSettings.value = relevantSettings
      return
    }
  }

  campaignLog.value = { keys: [], counts: {}, sets: {}, options: []}
  campaignSettings.value = []
}, { immediate: true })

// remove any associated recorded sets or keys for settings that are no longer valid
const filterSettings = function() {
  const invalidSettings = campaignSettings.value.flatMap((s) => settingActive(s) ? s.settings.filter((t) => !settingActive(t)) : s.settings)
  const invalidKeys = invalidSettings.flatMap((s) => {
    if (s.type === 'SetKey') {
      return [s.ckey]
    }
    if (s.type === 'ForceKey') {
      return [s.key]
    }
    return []
  })

  const invalidSetValues = invalidSettings.reduce((sets, s) => {
    if (s.type === 'ChooseRecordable') {
      const currentSet = sets[s.ckey] || []
      const newEntries = s.content.map((c) => c.content)
      return { ...sets, [s.ckey]: [...currentSet, ...newEntries] }
    }
    if (s.type === 'ForceRecorded') {
      const currentSet = sets[s.key] || []
      return { ...sets, [s.key]: [...currentSet, s.content] }
    }
    return sets
  }, {})

  const newKeys = campaignLog.value.keys.filter((k) => !invalidKeys.includes(k))

  const newSets = Object.fromEntries(Object.entries(campaignLog.value.sets).map(([k,v]) => {
    return [k, { ...v, entries: v.entries.filter((e) => !(invalidSetValues[k] || []).includes(e.value))}]
  }))

  const newCounts = campaignLog.value.counts
  const newOptions = campaignLog.value.options

  campaignLog.value = { keys: newKeys, sets: newSets, counts: newCounts, options: newOptions }
}

const setKey = function(setting: CampaignSetting, value: string) {
  const current = campaignLog.value
  if (setting.type === 'ChooseKey') {
    const keysToRemove = setting.content.filter((k) => k !== value)
    campaignLog.value = { ...current, keys: [...current.keys.filter((k) => !keysToRemove.includes(k)), value] }
    filterSettings()
  }
}

const setOption = function(setting: CampaignSetting, value: string) {
  const current = campaignLog.value
  if (setting.type === 'ChooseOption') {
    const keysToRemove = setting.content.filter((k) => k !== value)
    campaignLog.value = { ...current, options: [...current.options.filter((k) => !keysToRemove.includes(k)), value] }
    filterSettings()
  }
}

const setNum = function(setting: CampaignSetting, value: number) {
  const current = campaignLog.value
  if (setting.type === 'ChooseNum') {
    campaignLog.value = { ...current, counts: {...current.counts, [setting.ckey]: value}}
  }
}

const settingActive = function(setting: CampaignSetting) {
  const {ifRecorded, anyRecorded} = setting
  if (ifRecorded) {
    for (const condition of ifRecorded) {
      if (condition.type === 'key') {
        if (!campaignLog.value.keys.includes(condition.key)) {
          return false
        }
      } else if (condition.type === 'inSet') {
        const set = campaignLog.value.sets[condition.key]
        if (!set || !set.entries.find((e) => e.value === condition.content && e.tag === 'Recorded')) {
          return false
        }
      } else if (condition.type === 'count') {
        const count = campaignLog.value.counts[condition.key]
        if (condition.predicate.type === 'lte') {
          if (count === undefined || count > condition.predicate.value) {
            return false
          }
        } else if (condition.predicate.type === 'gte') {
          if (count === undefined || count < condition.predicate.value) {
            return false
          }
        }
      } else if(condition.type === 'option') {
        if (!campaignLog.value.options.includes(condition.key)) {
          return false
        }
      }
    }
  }

  if (anyRecorded) {
    let found = false
    for (const condition of anyRecorded) {
      if (condition.type === 'key') {
        if (campaignLog.value.keys.includes(condition.key)) {
          found = true
        }
      } else if (condition.type === 'inSet') {
        const set = campaignLog.value.sets[condition.key]
        if (set && set.entries.find((e) => e.value === condition.content)) {
          found = true
        }
      }
    }

    if (!found) {
      return false
    }
  }

  return true
}

const toggleKey = function(key: string) {
  const current = campaignLog.value
  if (current.keys.includes(key)) {
    current.keys = current.keys.filter((k) => k !== key)
  } else {
    current.keys.push(key)
  }

  filterSettings()
}

const toggleOption = function(key: string) {
  const current = campaignLog.value
  if (current.options.includes(key)) {
    current.options = current.options.filter((k) => k !== key)
  } else {
    current.options.push(key)
  }

  filterSettings()
}

const toggleSet = function(setting) {
  if(setting.type === 'SetRecordable') {
    const current = campaignLog.value
    const set = current.sets[setting.ckey]
    if (set) {
      const entry = set.entries.find((e) => e.value === setting.content)
      if (entry) {
        set.entries = set.entries.filter((e) => e.value !== setting.content)
      } else {
        set.entries.push({ tag: "Recorded", value: setting.content })
      }

    } else {
      campaignLog.value = { ...current, sets: {...current.sets, [setting.ckey]: { recordable: setting.recordable, entries: [{ tag: "Recorded", value: setting.content }]}}}
    }
  }

  filterSettings()
}

const chooseRecordable = function(setting, value) {
  if(setting.type === 'ChooseRecordable') {
    const current = campaignLog.value
    const set = current.sets[setting.ckey]
    if (set) {
      const keysToRemove = setting.content.filter((k) => k.content !== value).map((k) => k.content)
      set.entries = [...set.entries.filter((e) => !keysToRemove.includes(e.value)), { tag: "Recorded", value }]
    } else {
      campaignLog.value = { ...current, sets: {...current.sets, [setting.ckey]: { recordable: setting.recordable, entries: [{ tag: "Recorded", value }]}}}
    }
  }

  filterSettings()
}

const inSet = function(key: string, value: string) {
  const current = campaignLog.value
  const set = current.sets[key]
  if (set) {
    return set.entries.find((e) => e.value === value) !== undefined
  }

  return false
}

const isRecorded = function (value: string) {
  const current = campaignLog.value
  return current.keys.includes(value)
}

const isOption = function (value: string) {
  const current = campaignLog.value
  return current.options.includes(value)
}

const crossedOut = function (key: string, value: string) {
  const current = campaignLog.value
  return current.sets[key]?.entries.find((e) => e.value === value)?.tag === "CrossedOut"
}

const toggleCrossOut = function (key: string, value: string) {
  if (campaignLog.value.sets[key] === undefined) {
    return
  }

  const {entries} = campaignLog.value.sets[key]

  if (entries) {
    entries.map((e) => e.tag = e.value === value ? (e.tag === "Recorded" ? "CrossedOut" : "Recorded") : e.tag)
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
          <div class="options">
            <input type="radio" v-model="gameMode" :value="'Campaign'" id="campaign"> <label for="campaign">Campaign</label>
            <input type="radio" v-model="gameMode" :value="'Standalone'" id="standalone"> <label for="standalone">Standalone</label>
            <input type="radio" v-model="gameMode" :value="'SideStory'" id="sideStory"> <label for="sideStory">Side Story</label>
          </div>

          <div v-if="gameMode === 'SideStory'">
            <div class="scenarios">
              <div v-for="scenario in sideStories" :key="scenario.id">
                <img class="scenario-box" :class="{ 'selected-scenario': selectedScenario == scenario.id }" :src="imgsrc(`boxes/${scenario.id}.jpg`)" @click="selectedScenario = scenario.id">
              </div>
            </div>
          </div>
          <div v-else>
            <!-- <select v-model="selectedCampaign"> -->
              <div class="campaigns">
                <div v-for="campaign in campaigns" :key="campaign.id">
                  <img class="campaign-box" :class="{ 'selected-campaign': selectedCampaign == campaign.id }" :src="imgsrc(`boxes/${campaign.id}.jpg`)" @click="selectCampaign(campaign.id)">
                </div>
              </div>
            <!-- </select> -->
          </div>

          <div v-if="gameMode === 'Campaign' && selectedCampaign && selectedCampaignReturnToId" class="options">
            <input type="radio" v-model="returnTo" :value="false" id="normal"> <label for="normal">Normal</label>
            <input type="radio" v-model="returnTo" :value="true" id="returnTo"> <label for="returnTo">Return to...</label>
          </div>

          <div v-if="gameMode === 'Campaign' && selectedCampaign && campaignScenarios.length > 0" class="options">
            <input type="radio" v-model="fullCampaign" :value="true" id="full"> <label for="full">Full Campaign</label>
            <input type="radio" v-model="fullCampaign" :value="false" id="partial"> <label for="partial">Partial Campaign</label>
          </div>

          <div v-if="(gameMode === 'Standalone' || !fullCampaign) && selectedCampaign">
            <div class="scenarios">
              <div v-for="scenario in campaignScenarios" :key="scenario.id">
                <img class="scenario-box" :class="{ 'selected-scenario': selectedScenario == scenario.id }" :src="imgsrc(`boxes/${scenario.id}.jpg`)" @click="selectedScenario = scenario.id">
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




          <div v-if="standaloneSettings.length > 0">
            <p>Standalone Settings</p>
            <div v-for="setting in standaloneSettings" :key="setting.key">
              <div v-if="setting.type === 'ToggleKey'" class="options">
                <input type="checkbox" v-model="setting.content" :id="setting.key"/>
                <label :for="setting.key"> {{toCapitalizedWords(setting.key)}}</label>
              </div>
              <div v-else>
                {{toCapitalizedWords(setting.key)}}
                <div class="options">
                  <template v-for="option in setting.content" :key="option.key">
                    <input type="checkbox" v-model="option.content" :id="`${option.key}${option.value}`" class="invert" />
                    <label :for="`${option.key}${option.value}`">
                      <s v-if="option.content">{{option.label}}</s>
                      <span v-else>{{option.label}}</span>
                    </label>
                  </template>
                </div>
              </div>
            </div>
          </div>

          <div v-if="!fullCampaign && campaignSettings.length > 0">
            <p>Campaign Settings</p>
            <div v-for="setting in campaignSettings" :key="setting.key">
              <div v-if="settingActive(setting)" class="campaign-settings">
                {{setting.key}}
                <div v-for="setting in setting.settings" :key="setting.key">
                  <template v-if="settingActive(setting)">
                    <div v-if="setting.type === 'SetKey'">
                      <div class="options">
                        <input type="checkbox" :name="setting.key" :id="setting.key" @change.prevent="toggleKey(setting.ckey)" :checked="isRecorded(setting.ckey)" />
                        <label :for="setting.key">{{toCapitalizedWords(setting.key)}}</label>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'Option'">
                      <div class="options">
                        <input type="checkbox" :name="setting.key" :id="setting.key" @change.prevent="toggleOption(setting.ckey)" :checked="isOption(setting.ckey)" />
                        <label :for="setting.key">{{toCapitalizedWords(setting.key)}}</label>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'ForceKey'">
                      <div class="options">
                        <input type="checkbox" :name="setting.key" :id="setting.key" checked disabled />
                        <label :for="setting.key">{{toCapitalizedWords(setting.key)}}</label>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'ForceRecorded'">
                      <div class="options">
                        <input type="checkbox" :name="setting.key" :id="setting.key" checked disabled />
                        <label :for="setting.key">{{toCapitalizedWords(setting.key)}}</label>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'ChooseKey'">
                      {{toCapitalizedWords(setting.key)}}
                      <div class="options">
                        <template v-for="option in setting.content" :key="option.key">
                          <input type="radio" :value="option" :name="setting.key" :id="option" @change.prevent="setKey(setting, option)" :checked="isRecorded(option)" />
                          <label :for="option">{{toCapitalizedWords(option)}}</label>
                        </template>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'ChooseOption'">
                      {{toCapitalizedWords(setting.key)}}
                      <div class="options">
                        <template v-for="option in setting.content" :key="option.key">
                          <input type="radio" :value="option" :name="setting.key" :id="option" @change.prevent="setOption(setting, option)" :checked="isOption(option)" />
                          <label :for="option">{{toCapitalizedWords(option)}}</label>
                        </template>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'CrossOut'">
                      {{toCapitalizedWords(setting.key)}}
                      <div class="options">
                        <template v-for="option in setting.content" :key="option.key">
                          <input type="checkbox" :name="`${setting.key}${option.key}`" :id="`${setting.key}${option.key}`" class="invert" @change.prevent="toggleCrossOut(setting.key, option.content)" :checked="crossedOut(setting.key, option.content)"/>
                          <label :for="`${setting.key}${option.key}`">
                            <s v-if="crossedOut(setting.key, option.content)">{{toCapitalizedWords(option.key)}}</s>
                            <span v-else>{{toCapitalizedWords(option.key)}}</span>
                          </label>
                        </template>
                      </div>
                    </div>
                    <div v-else-if="setting.type === 'ChooseNum'">
                      {{toCapitalizedWords(setting.key)}}
                      <input
                        type="number"
                        :name="setting.key"
                        :id="setting.key"
                        min="0"
                        :max="setting.max"
                        @change.prevent="setNum(setting, parseInt($event.target.value))"
                      />
                    </div>
                    <div v-else-if="setting.type === 'SetRecordable'" class="options">
                      <input type="checkbox" :name="setting.key" :id="setting.key" @change.prevent="toggleSet(setting)" :checked="inSet(setting.ckey, setting.content)" />
                      <label :for="setting.key">{{toCapitalizedWords(setting.key)}}</label>
                    </div>
                    <div v-else-if="setting.type === 'ChooseRecordable'">
                      {{toCapitalizedWords(setting.key)}}
                      <div class="options">
                        <template v-for="option in setting.content" :key="option.key">
                          <input type="radio" :name="setting.key" :id="`${setting.key}${option.key}`" @change.prevent="chooseRecordable(setting, option.content)"/>
                          <label :for="`${setting.key}${option.key}`">{{toCapitalizedWords(option.key)}}</label>
                        </template>
                      </div>
                    </div>
                    <div v-else>
                      {{setting.type}}
                    </div>
                  </template>
                </div>
              </div>
            </div>
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
  gap: 2px;

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
