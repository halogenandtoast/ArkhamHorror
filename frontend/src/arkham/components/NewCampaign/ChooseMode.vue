<script lang="ts" setup>
import type { Scenario, Campaign } from '@/arkham/data'
import { imgsrc } from '@/arkham/helpers'

type GameMode = 'Campaign' | 'SideStory'

defineProps<{
  campaigns: Campaign[]
  sideStories: Scenario[]
  campaign: Campaign | null | undefined
  scenario: Scenario | undefined
}>()

const gameMode = defineModel<GameMode>('gameMode', { required: true })
const selectedCampaign = defineModel<string | null>('selectedCampaign', { required: true })
const selectedScenario = defineModel<string | null>('selectedScenario', { required: true })

const emits = defineEmits(['go'])
</script>

<template>
  <div class="mode-toggle segmented segmented-2">
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
      >
        <div
          class="vt-box"
          :style="selectedScenario == s.id ? { 'view-transition-name': 'selected-game-box' } : {}"
          :class="{ beta: s.beta, alpha: s.alpha }"
        >
          <img
            class="scenario-box"
            :class="{ 'selected-scenario': selectedScenario == s.id }"
            :src="imgsrc(`boxes/${s.id}.jpg`)"
            @click="selectedScenario = s.id; emits('go')"
          />
        </div>
      </div>
    </div>
  </template>

  <template v-else>
    <div class="campaigns">
      <template v-for="c in campaigns" :key="c.id">
        <div class="campaign">
          <div
            class="vt-box"
            :style="selectedCampaign == c.id ? { 'view-transition-name': 'selected-game-box' } : {}"
            :class="{ beta: c.beta, alpha: c.alpha }"
          >
            <input
              type="image"
              class="campaign-box"
              :class="{ 'selected-campaign': selectedCampaign == c.id }"
              :src="imgsrc(`boxes/${c.id}.jpg`)"
              @click.prevent="selectedCampaign = c.id; emits('go')"
            />
          </div>
        </div>
      </template>
    </div>
  </template>
</template>

<style lang="css" scoped>
input[type='radio'] {
  display: none;
}

.segmented {
  display: grid;
  border-radius: 14px;
  overflow: hidden;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(0,0,0,0.18);
  box-shadow: 0 10px 26px rgba(0,0,0,0.25);
}

.segmented-2 { grid-template-columns: repeat(2, 1fr); }

.segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding-block: 10px;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-size: 13px;
  user-select: none;
  cursor: pointer;
  background: rgba(255,255,255,0.06);
  border-right: 1px solid rgba(255,255,255,0.08);
  transition: background 160ms ease, transform 120ms ease;
}

.segmented label:last-of-type {
  border-right: none;
}

.segmented label:hover {
  background: rgba(255,255,255,0.10);
}

input[type='radio']:checked + label {
  background: rgba(110, 134, 64, 0.95);
  box-shadow: inset 0 0 0 1px rgba(255,255,255,0.12);
}

.campaigns,
.scenarios {
  margin-top: 12px;
  display: grid;
  gap: 12px;
  line-height: 0;
}

.campaigns {
  grid-template-columns: repeat(6, 1fr);
}

.scenarios {
  grid-template-columns: repeat(6, 1fr);
}

@media (max-width: 1500px) {
  .campaigns, .scenarios {
    grid-template-columns: repeat(3, 1fr);
  }
}

.campaign,
.scenario {
  position: relative;
}

.vt-box {
  display: block;
  border-radius: 14px;
  position: relative;
  overflow: hidden;
  background: rgba(0,0,0,0.18);
  box-shadow: 0 10px 24px rgba(0,0,0,0.35);
  outline: 1px solid rgba(255,255,255,0.08);
  transition: transform 160ms ease, box-shadow 160ms ease, outline-color 160ms ease;
}

.vt-box:hover {
  transform: translateY(-2px);
  box-shadow: 0 16px 34px rgba(0,0,0,0.45);
  outline-color: rgba(255,255,255,0.14);
}

.campaign-box,
.scenario-box {
  width: 100%;
  display: block;
}

.campaign-box:not(.selected-campaign),
.scenario-box:not(.selected-scenario) {
  filter: grayscale(100%) contrast(1.05) brightness(0.95);
  transition: filter 220ms ease;
}

.vt-box:hover .campaign-box:not(.selected-campaign),
.vt-box:hover .scenario-box:not(.selected-scenario) {
  filter: grayscale(70%) contrast(1.08) brightness(0.98);
}

.selected-campaign,
.selected-scenario {
  filter: none !important;
}

.vt-box[style*="view-transition-name"] {
  outline-color: rgba(154, 196, 78, 0.55);
  box-shadow:
    0 18px 40px rgba(0,0,0,0.55),
    0 0 0 1px rgba(154, 196, 78, 0.25),
    0 0 24px rgba(154, 196, 78, 0.18);
}

.vt-box.beta:after,
.vt-box.alpha:after {
  position: absolute;
  z-index: 1070;
  width: 86px;
  height: 26px;
  top: 9px;
  left: -22px;
  text-align: center;
  font-size: 12px;
  letter-spacing: 0.08em;
  font-family: sans-serif;
  text-transform: uppercase;
  font-weight: 700;
  color: white;
  line-height: 28px;
  transform: rotate(-45deg);
  box-shadow: 0 10px 18px rgba(0,0,0,0.35);
}

.vt-box.beta:after {
  content: 'beta';
  background: darkgoldenrod;
}

.vt-box.alpha:after {
  content: 'alpha';
  background: darkred;
}

.beta-warning,
.alpha-warning {
  margin-top: 12px;
  padding: 12px;
  border-radius: 12px;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-size: 13px;
  border: 1px solid rgba(255,255,255,0.08);
  box-shadow: 0 10px 22px rgba(0,0,0,0.22);
}

.beta-warning {
  background: rgba(184, 134, 11, 0.25);
}

.alpha-warning {
  background: rgba(139, 0, 0, 0.25);
}

.mode-toggle {
  margin-bottom: 6px;
}
</style>
