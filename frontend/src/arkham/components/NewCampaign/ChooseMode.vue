<script lang="ts" setup>
import { computed } from 'vue'
import type { Scenario, Campaign } from '@/arkham/data'
import { imgsrc } from '@/arkham/helpers'

type GameMode = 'Campaign' | 'SideStory'

const props = defineProps<{
  campaigns: Campaign[]
  sideStories: Scenario[]
  campaign: Campaign | null | undefined
  scenario: Scenario | undefined
}>()

const gameMode = defineModel<GameMode>('gameMode', { required: true })
const selectedCampaign = defineModel<string | null>('selectedCampaign', { required: true })
const selectedScenario = defineModel<string | null>('selectedScenario', { required: true })

const showCampaignWarnings = computed(() => gameMode.value === 'Campaign' && !!props.campaign)
const showScenarioWarnings = computed(() => gameMode.value === 'SideStory' && !!props.scenario)
</script>

<template>
  <div class="options">
    <input type="radio" v-model="gameMode" :value="'Campaign'" id="campaign" />
    <label for="campaign">{{ $t('create.campaign') }}</label>

    <input type="radio" v-model="gameMode" :value="'SideStory'" id="sideStory" />
    <label for="sideStory">{{ $t('create.sideStory') }}</label>
  </div>

  <template v-if="gameMode === 'SideStory'">
    <div class="scenarios">
      <div
        v-for="s in sideStories"
        :key="s.id"
        class="scenario"
        :class="{ beta: s.beta, alpha: s.alpha }"
      >
        <div
          class="vt-box"
          :style="selectedScenario == s.id ? { 'view-transition-name': 'selected-game-box' } : {}"
        >
          <img
            class="scenario-box"
            :class="{ 'selected-scenario': selectedScenario == s.id }"
            :src="imgsrc(`boxes/${s.id}.jpg`)"
            @click="selectedScenario = s.id"
          />
        </div>
      </div>
    </div>

    <div class="beta-warning" v-if="showScenarioWarnings && scenario?.beta">
      {{ $t('create.betaWarningScenario') }}
    </div>
  </template>

  <template v-else>
    <div class="campaigns">
      <template v-for="c in campaigns" :key="c.id">
        <div class="campaign" :class="{ beta: c.beta, alpha: c.alpha }">
          <div
            class="vt-box"
            :style="selectedCampaign == c.id ? { 'view-transition-name': 'selected-game-box' } : {}"
          >
            <input
              type="image"
              class="campaign-box"
              :class="{ 'selected-campaign': selectedCampaign == c.id }"
              :src="imgsrc(`boxes/${c.id}.jpg`)"
              @click.prevent="selectedCampaign = c.id"
            />
          </div>
        </div>
      </template>
    </div>

    <div class="alpha-warning" v-if="showCampaignWarnings && campaign?.alpha">
      {{ $t('create.alphaWarning') }}
    </div>
    <div class="beta-warning" v-if="showCampaignWarnings && campaign?.beta">
      {{ $t('create.betaWarning') }}
    </div>
  </template>
</template>

<style lang="css" scoped>
h2 {
  color: #cecece;
  margin-left: 10px;
  text-transform: uppercase;
  font-family: Teutonic;
  font-size: 2em;
  padding: 0;
  margin: 0;
}

header {
  display: flex;
  align-items: center;
  justify-content: center;
}

header h2 {
  flex: 1;
}

input[type='radio'] {
  display: none;
}

input[type='radio'] + label {
  display: inline-block;
  padding: 4px 12px;
  background-color: hsl(80, 5%, 39%);
  border-color: #ddd;
}

input[type='radio'] + label:hover {
  background-color: hsl(80, 15%, 39%);
}

input[type='radio']:checked + label {
  background: #6e8640;
}

input[type='image'] {
  width: 100%;
}

.options {
  display: flex;
}

.options label {
  flex: 1;
  text-align: center;
  margin-left: 10px;
}

.options label:nth-of-type(1) {
  margin-left: 0;
}

.campaigns {
  display: grid;
  gap: 10px;
  line-height: 0;
  grid-template-columns: repeat(6, auto);
}

@media (max-width: 1500px) {
  .campaigns {
    grid-template-columns: repeat(3, auto);
  }
}

.campaign-box:not(.selected-campaign) {
  filter: grayscale(100%);
}

.scenarios {
  display: grid;
  line-height: 0;
  grid-template-columns: repeat(4, 1fr);
  gap: 10px;
  margin-bottom: 10px;
}

.scenario-box:not(.selected-scenario) {
  filter: grayscale(100%) sepia(0);
  transition: filter 1s linear;
}

.scenario-box:not(.selected-scenario):hover {
  filter: grayscale(100%) sepia(1);
  transition: filter 1s linear;
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

.campaign,
.scenario {
  position: relative;
  overflow: hidden;
}

.campaign.beta:after,
.scenario.beta:after {
  content: 'beta';
  position: absolute;
  z-index: 1070;
  width: 80px;
  height: 25px;
  background: darkgoldenrod;
  top: 7px;
  left: -20px;
  text-align: center;
  font-size: 12px;
  letter-spacing: 1px;
  font-family: sans-serif;
  text-transform: uppercase;
  font-weight: bold;
  color: white;
  line-height: 27px;
  transform: rotate(-45deg);
}

.campaign.alpha:after,
.scenario.alpha:after {
  content: 'alpha';
  position: absolute;
  z-index: 1070;
  width: 80px;
  height: 25px;
  background: darkred;
  top: 7px;
  left: -20px;
  text-align: center;
  font-size: 12px;
  letter-spacing: 1px;
  font-family: sans-serif;
  text-transform: uppercase;
  font-weight: bold;
  color: white;
  line-height: 27px;
  transform: rotate(-45deg);
}

.beta-warning {
  font-size: 1.2em;
  text-transform: uppercase;
  padding: 10px;
  background: darkgoldenrod;
  margin-block: 10px;
}

.alpha-warning {
  font-size: 1.2em;
  text-transform: uppercase;
  padding: 10px;
  background: darkred;
  margin-block: 10px;
}

.vt-box {
  display: block;
  line-height: 0;
}
</style>
