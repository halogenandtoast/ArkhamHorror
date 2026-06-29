<script lang="ts" setup>
import { computed } from 'vue'
import type { Scenario, Campaign } from '@/arkham/data'
import { imgsrc } from '@/arkham/helpers'

type GameMode = 'Campaign' | 'SideStory'

const CHAPTER_2_CAMPAIGN_IDS = new Set(['12'])

const props = defineProps<{
  campaigns: Campaign[]
  sideStories: Scenario[]
  campaign: Campaign | null | undefined
  scenario: Scenario | undefined
}>()

const gameMode = defineModel<GameMode>('gameMode', { required: true })
const selectedCampaign = defineModel<string | null>('selectedCampaign', { required: true })
const selectedScenario = defineModel<string | null>('selectedScenario', { required: true })

const emits = defineEmits(['go'])

const chapter1Campaigns = computed(() =>
  props.campaigns.filter((c) => !CHAPTER_2_CAMPAIGN_IDS.has(c.id))
)
const chapter2Campaigns = computed(() =>
  props.campaigns.filter((c) => CHAPTER_2_CAMPAIGN_IDS.has(c.id))
)
const isChallengeScenario = (scenario: Scenario) =>
  Boolean(scenario.requiredInvestigator) || Boolean(scenario.deckRequirements?.length)

const sideStoryScenarios = computed(() =>
  props.sideStories.filter((s) => !isChallengeScenario(s))
)
const challengeScenarios = computed(() =>
  props.sideStories.filter(isChallengeScenario)
)

function withViewTransition(fn: () => void) {
  const d = document as Document & { startViewTransition?: (callback: () => void) => void }
  if (typeof d.startViewTransition === 'function') {
    d.startViewTransition(fn)
  } else {
    fn()
  }
}

function selectGameMode(mode: 'Campaign' | 'SideStory') {
  if (gameMode.value === mode) return
  withViewTransition(() => {
    gameMode.value = mode
  })
}
</script>

<template>
  <div class="mode-toggle segmented segmented-2">
    <input type="radio" :checked="gameMode === 'Campaign'" id="campaign" @change="selectGameMode('Campaign')" />
    <label for="campaign">{{ $t('create.campaign') }}</label>

    <input type="radio" :checked="gameMode === 'SideStory'" id="sideStory" @change="selectGameMode('SideStory')" />
    <label for="sideStory">{{ $t('create.sideStory') }}</label>
  </div>

  <template v-if="gameMode === 'SideStory'">
    <section v-if="sideStoryScenarios.length" class="chapter">
      <header class="chapter-header">
        <span class="chapter-line" />
        <h3 class="chapter-title">{{ $t('create.sideStoriesHeading') }}</h3>
        <span class="chapter-line" />
      </header>
      <div class="scenarios">
        <div
          v-for="s in sideStoryScenarios"
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
    </section>

    <section v-if="challengeScenarios.length" class="chapter">
      <header class="chapter-header">
        <span class="chapter-line" />
        <h3 class="chapter-title">{{ $t('create.challengeScenariosHeading') }}</h3>
        <span class="chapter-line" />
      </header>
      <div class="scenarios">
        <div
          v-for="s in challengeScenarios"
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
          <span v-if="s.requiredInvestigator" class="requires-investigator">
            {{ $t('create.requiresInvestigator', { name: s.requiredInvestigator }) }}
          </span>
          <span v-for="requirement in s.deckRequirements" :key="requirement" class="requires-investigator">
            {{ requirement }}
          </span>
        </div>
      </div>
    </section>
  </template>

  <template v-else>
    <section v-if="chapter1Campaigns.length" class="chapter">
      <header class="chapter-header">
        <span class="chapter-line" />
        <h3 class="chapter-title">{{ $t('create.chapter1Heading') }}</h3>
        <span class="chapter-line" />
      </header>
      <div class="campaigns">
        <template v-for="c in chapter1Campaigns" :key="c.id">
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
    </section>

    <section v-if="chapter2Campaigns.length" class="chapter">
      <header class="chapter-header">
        <span class="chapter-line" />
        <h3 class="chapter-title">{{ $t('create.chapter2Heading') }}</h3>
        <span class="chapter-line" />
      </header>
      <div class="campaigns">
        <template v-for="c in chapter2Campaigns" :key="c.id">
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
    </section>
  </template>
</template>

<style lang="css" scoped>
input[type='radio'] {
  display: none;
}

.segmented {
  --segmented-gap: 2px;
  --segmented-padding: 2px;
  display: grid;
  border-radius: 5px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  padding: var(--segmented-padding);
  gap: var(--segmented-gap);
  position: relative;
}

.segmented::before {
  content: '';
  background: var(--button-1);
  border-radius: 3px;
  bottom: var(--segmented-padding);
  left: var(--segmented-padding);
  position: absolute;
  top: var(--segmented-padding);
  transform: translateX(0);
  transition: transform 220ms cubic-bezier(.2, .8, .2, 1), background 150ms ease;
  width: calc((100% - (var(--segmented-padding) * 2) - var(--segmented-gap)) / 2);
  z-index: 0;
}

.segmented:has(#sideStory:checked)::before {
  transform: translateX(calc(100% + var(--segmented-gap)));
}

@supports (left: anchor(left)) {
  #campaign:checked + label,
  #sideStory:checked + label {
    anchor-name: --active-game-mode;
  }

  .segmented::before {
    bottom: auto;
    height: anchor-size(height);
    left: anchor(left);
    position-anchor: --active-game-mode;
    top: anchor(top);
    transform: none;
    transition:
      left 220ms cubic-bezier(.2, .8, .2, 1),
      top 220ms cubic-bezier(.2, .8, .2, 1),
      width 220ms cubic-bezier(.2, .8, .2, 1),
      height 220ms cubic-bezier(.2, .8, .2, 1),
      background 150ms ease;
    width: anchor-size(width);
  }

  .segmented:has(#sideStory:checked)::before {
    transform: none;
  }
}

.segmented-2 { grid-template-columns: repeat(2, 1fr); }

.segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 6px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 11px;
  font-weight: 600;
  user-select: none;
  cursor: pointer;
  border-radius: 3px;
  color: var(--background-light);
  margin: 0;
  position: relative;
  transition: color 0.15s ease;
  z-index: 1;
}

.segmented label:hover {
  color: var(--text);
}

input[type='radio']:checked + label {
  color: var(--text);
}

.segmented:hover::before {
  background: var(--button-1-highlight);
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

.requires-investigator {
  display: block;
  margin-top: 8px;
  line-height: 1.2;
  text-align: center;
  color: rgba(206, 206, 206, 0.88);
  font-size: 12px;
  letter-spacing: 0.04em;
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
  z-index: var(--z-index-1070);
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

.chapter {
  margin-top: 18px;
}

.chapter:first-of-type {
  margin-top: 12px;
}

.chapter-header {
  display: flex;
  align-items: center;
  gap: 14px;
  margin-bottom: 6px;
}

.chapter-line {
  flex: 1;
  height: 1px;
  background: linear-gradient(
    to right,
    rgba(255, 255, 255, 0) 0%,
    rgba(255, 255, 255, 0.18) 50%,
    rgba(255, 255, 255, 0) 100%
  );
}

.chapter-title {
  margin: 0;
  padding: 0 4px;
  font-family: Teutonic, serif;
  font-size: 1.4em;
  font-weight: 400;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: rgba(206, 206, 206, 0.92);
  text-shadow: 0 2px 8px rgba(0, 0, 0, 0.45);
  white-space: nowrap;
}
</style>
