<script lang="ts" setup>
import { computed, ref, watch } from 'vue'
import type { Scenario, Campaign } from '@/arkham/data'
import { imgsrc } from '@/arkham/helpers'

type GameMode = 'Campaign' | 'SideStory'
type CampaignGroup = 'chapter1' | 'chapter2' | 'homebrew'
type ScenarioGroup = 'sideStories' | 'challengeScenarios'

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
const campaignGroup = defineModel<CampaignGroup>('campaignGroup', { required: true })
const scenarioGroup = ref<ScenarioGroup>('sideStories')
const emits = defineEmits(['go'])

const isChapter2 = (id: string) => id.startsWith('12')
const isHomebrew = (id: string) => id.startsWith(':')
const isChallengeScenario = (scenario: Scenario) =>
  Boolean(scenario.requiredInvestigator) || Boolean(scenario.deckRequirements?.length)

const chapter1Campaigns = computed(() =>
  props.campaigns.filter((c) => !CHAPTER_2_CAMPAIGN_IDS.has(c.id) && !c.homebrew)
)
const chapter2Campaigns = computed(() =>
  props.campaigns.filter((c) => CHAPTER_2_CAMPAIGN_IDS.has(c.id))
)
const homebrewCampaigns = computed(() =>
  props.campaigns.filter((c) => c.homebrew)
)
const chapter1SideStories = computed(() =>
  props.sideStories.filter((s) => !isChapter2(s.id) && !isHomebrew(s.id))
)
const chapter2SideStories = computed(() =>
  props.sideStories.filter((s) => isChapter2(s.id))
)
const homebrewSideStories = computed(() =>
  props.sideStories.filter((s) => isHomebrew(s.id))
)

// Homebrew box art may not be present yet; fall back to a styled placeholder tile.
const missingBoxArt = ref<Record<string, boolean>>({})

function campaignBoxSrc(campaign: Campaign) {
  if (!campaign.homebrew) return imgsrc(`boxes/${campaign.id}.jpg`)
  const homebrewId = campaign.id.replace(/^:/, '')
  return imgsrc(`homebrew/${homebrewId}/boxes/${homebrewId}.jpg`)
}

const chapterGroups = computed(() => [
  {
    id: 'chapter1' as const,
    label: 'create.chapter1Heading',
    campaigns: chapter1Campaigns.value,
    sideStories: chapter1SideStories.value,
  },
  {
    id: 'chapter2' as const,
    label: 'create.chapter2Heading',
    campaigns: chapter2Campaigns.value,
    sideStories: chapter2SideStories.value,
  },
  ...(!import.meta.env.PROD ? [{
    id: 'homebrew' as const,
    label: 'create.homebrewHeading',
    campaigns: homebrewCampaigns.value,
    sideStories: homebrewSideStories.value,
  }] : []),
].filter((group) => group.campaigns.length || group.sideStories.length))

const activeGroup = computed(() =>
  chapterGroups.value.find((group) => group.id === campaignGroup.value) ?? chapterGroups.value[0]
)
const activeCampaigns = computed(() => activeGroup.value?.campaigns ?? [])
const allActiveScenarios = computed(() => activeGroup.value?.sideStories ?? [])
const scenarioGroups = computed(() => [
  {
    id: 'sideStories' as const,
    label: 'create.sideStoriesHeading',
    items: allActiveScenarios.value.filter((scenario) => !isChallengeScenario(scenario)),
  },
  {
    id: 'challengeScenarios' as const,
    label: 'create.challengeScenariosHeading',
    items: allActiveScenarios.value.filter(isChallengeScenario),
  },
].filter((group) => group.items.length))
const activeScenarios = computed(() =>
  scenarioGroups.value.find((group) => group.id === scenarioGroup.value)?.items ?? scenarioGroups.value[0]?.items ?? []
)
const hasCampaigns = computed(() => activeCampaigns.value.length > 0)
const hasSideStories = computed(() => allActiveScenarios.value.length > 0)

watch(campaignGroup, () => {
  if (gameMode.value === 'Campaign' && !hasCampaigns.value && hasSideStories.value) {
    gameMode.value = 'SideStory'
  } else if (gameMode.value === 'SideStory' && !hasSideStories.value && hasCampaigns.value) {
    gameMode.value = 'Campaign'
  }
})

watch([campaignGroup, gameMode], () => {
  if (!scenarioGroups.value.some((group) => group.id === scenarioGroup.value)) {
    scenarioGroup.value = scenarioGroups.value[0]?.id ?? 'sideStories'
  }
})

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
  <div class="chapter-select" :style="{ '--item-count': chapterGroups.length }">
    <template v-for="group in chapterGroups" :key="group.id">
      <input :id="`chapter-${group.id}`" v-model="campaignGroup" type="radio" :value="group.id" />
      <label :for="`chapter-${group.id}`">{{ $t(group.label) }}</label>
    </template>
  </div>

  <div class="mode-toggle segmented segmented-2">
    <input
      id="campaign"
      type="radio"
      :checked="gameMode === 'Campaign'"
      :disabled="!hasCampaigns"
      @change="selectGameMode('Campaign')"
    />
    <label for="campaign">{{ $t('create.campaign') }}</label>

    <input
      id="sideStory"
      type="radio"
      :checked="gameMode === 'SideStory'"
      :disabled="!hasSideStories"
      @change="selectGameMode('SideStory')"
    />
    <label for="sideStory">{{ $t('create.sideStory') }}</label>
  </div>

  <div
    v-if="gameMode === 'SideStory' && scenarioGroups.length > 1"
    class="scenario-select"
    :style="{ '--item-count': scenarioGroups.length }"
  >
    <template v-for="group in scenarioGroups" :key="group.id">
      <input :id="`scenario-${group.id}`" v-model="scenarioGroup" type="radio" :value="group.id" />
      <label :for="`scenario-${group.id}`">{{ $t(group.label) }}</label>
    </template>
  </div>

  <template v-if="gameMode === 'SideStory'">
    <div class="scenarios">
      <div
        v-for="s in activeScenarios"
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
  </template>
  <template v-else>
    <div v-if="campaignGroup === 'homebrew'" class="homebrew-warning">
      If you are seeing this, do not start one of these campaigns, they will break.
    </div>

    <div class="campaigns">
      <template v-for="c in activeCampaigns" :key="c.id">
        <div class="campaign">
          <div
            class="vt-box"
            :style="selectedCampaign == c.id ? { 'view-transition-name': 'selected-game-box' } : {}"
            :class="{ beta: c.beta, alpha: c.alpha }"
          >
            <input
              v-if="!c.homebrew || !missingBoxArt[c.id]"
              type="image"
              class="campaign-box"
              :class="{ 'selected-campaign': selectedCampaign == c.id }"
              :src="campaignBoxSrc(c)"
              @error="missingBoxArt[c.id] = true"
              @click.prevent="selectedCampaign = c.id; emits('go')"
            />
            <button
              v-else
              type="button"
              class="campaign-box campaign-box-placeholder"
              :class="{ 'selected-campaign': selectedCampaign == c.id }"
              @click.prevent="selectedCampaign = c.id; emits('go')"
            >
              {{ c.name }}
            </button>
          </div>
          <span v-if="c.designer" class="designer-credit">
            {{ $t('create.designedBy', { name: c.designer }) }}
          </span>
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

.chapter-select {
  display: grid;
  grid-template-columns: repeat(var(--item-count), 1fr);
  gap: 6px;
  margin-bottom: 10px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
}

.chapter-select label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 8px 10px;
  border-bottom: 2px solid transparent;
  color: var(--background-light);
  cursor: pointer;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  transition: border-color 0.15s ease, color 0.15s ease;
}

.chapter-select label:hover,
.chapter-select input[type='radio']:checked + label {
  border-bottom-color: var(--button-1);
  color: var(--text);
}

.scenario-select {
  display: grid;
  grid-template-columns: repeat(var(--item-count), 1fr);
  gap: 4px;
  width: min(100%, 560px);
  margin: 14px auto 4px;
  padding: 4px;
  border: 1px solid rgba(255, 255, 255, 0.09);
  border-radius: 7px;
  background: rgba(0, 0, 0, 0.18);
  box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.2);
}

.scenario-select label {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 8px;
  min-height: 34px;
  padding: 6px 14px;
  border: 1px solid transparent;
  border-radius: 4px;
  color: rgba(206, 206, 206, 0.58);
  font-family: Teutonic, sans-serif;
  font-size: clamp(0.76rem, 1.1vw, 0.9rem);
  text-align: center;
  cursor: pointer;
  transition: background-color 0.15s ease, border-color 0.15s ease, color 0.15s ease, box-shadow 0.15s ease;
}

.scenario-select label::before {
  width: 6px;
  height: 6px;
  flex: 0 0 6px;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 50%;
  content: '';
  transition: background-color 0.15s ease, border-color 0.15s ease;
}

.scenario-select label:hover {
  color: rgba(255, 255, 255, 0.82);
  background: rgba(255, 255, 255, 0.035);
}

.scenario-select input[type='radio']:checked + label {
  border-color: rgba(255, 255, 255, 0.08);
  color: var(--text);
  background: rgba(255, 255, 255, 0.09);
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.24);
}

.scenario-select input[type='radio']:checked + label::before {
  border-color: var(--button-1);
  background: var(--button-1);
}

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

.segmented input[type='radio']:disabled + label {
  color: rgba(206, 206, 206, 0.3);
  cursor: not-allowed;
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
  filter: none;
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
.alpha-warning,
.homebrew-warning {
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

.alpha-warning,
.homebrew-warning {
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

.campaign-box-placeholder {
  aspect-ratio: 1 / 1;
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  padding: 12px;
  margin: 0;
  border: 1px dashed rgba(255, 255, 255, 0.18);
  border-radius: 14px;
  background:
    radial-gradient(circle at 50% 35%, rgba(255, 255, 255, 0.06), rgba(0, 0, 0, 0) 65%),
    var(--background-dark, #1a1a1a);
  color: rgba(206, 206, 206, 0.92);
  font-family: Teutonic, serif;
  font-size: 1.6em;
  line-height: 1.2;
  letter-spacing: 0.08em;
  cursor: pointer;
  transition: border-color 160ms ease, color 160ms ease;
}

/* placeholder tile is actionable: magenta --select per the color-role convention */
.campaign-box-placeholder:hover,
.campaign-box-placeholder.selected-campaign {
  border-color: var(--select, magenta);
  color: var(--title, #fff);
}

.designer-credit {
  display: block;
  margin-top: 8px;
  line-height: 1.2;
  text-align: center;
  color: rgba(206, 206, 206, 0.7);
  font-size: 12px;
  font-style: italic;
  letter-spacing: 0.04em;
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
