<script lang="ts" setup>
import { watch, ref, computed, onMounted, onUnmounted } from 'vue'
import { useUserStore } from '@/stores/user'
import { useRoute, useRouter } from 'vue-router'
import * as Arkham from '@/arkham/types/Deck'
import { fetchDecks, newGame } from '@/arkham/api'
import type { Difficulty } from '@/arkham/types/Difficulty'
import type { Scenario, Campaign } from '@/arkham/data'
import { storeToRefs } from 'pinia'

import campaignJSON from '@/arkham/data/campaigns'
import scenarioJSON from '@/arkham/data/scenarios'
import sideStoriesJSON from '@/arkham/data/side-stories'

import ChooseMode from '@/arkham/components/NewCampaign/ChooseMode.vue'
import GameOptions from '@/arkham/components/NewCampaign/GameOptions.vue'

type Gateable = { alpha?: boolean; beta?: boolean; dev?: boolean }
type GameMode = 'Campaign' | 'SideStory'
type MultiplayerVariant = 'WithFriends' | 'TrueSolo'
type CampaignType = 'FullCampaign' | 'PartialCampaign' | 'Standalone'

const store = useUserStore()
const { currentUser } = storeToRefs(store)

const route = useRoute()
const router = useRouter()

const dev = import.meta.env.PROD ? false : true
const alpha = ref(false)
const isBetaUser = computed(() => !!currentUser.value?.beta)

const gate = <T extends Gateable>(items: T[]) =>
  items.filter((x) => {
    if (x.dev) return dev && alpha.value
    if (x.beta) return isBetaUser.value
    if (x.alpha) return alpha.value
    return true
  })

type Step = 'ChooseMode' | 'GameOptions'

const step = ref<Step>('ChooseMode')

// ----- state (shared) -----
const gameMode = ref<GameMode>('Campaign')
const includeTarotReadings = ref(false)
const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)

const playerCount = ref(1)
const selectedDifficulty = ref<Difficulty>('Easy')
const deckIds = ref<(string | null)[]>([null, null, null, null])

const fullCampaign = ref<CampaignType>('FullCampaign')
const selectedCampaign = ref<string | null>(null)
const selectedScenario = ref<string | null>(null)
const campaignName = ref<string | null>(null)
const multiplayerVariant = ref<MultiplayerVariant>('WithFriends')
const returnTo = ref(false)

// ----- data -----
const scenarios = computed<Scenario[]>(() => gate(scenarioJSON))
const sideStories = computed<Scenario[]>(() => gate(sideStoriesJSON))
const campaigns = computed<Campaign[]>(() => gate(campaignJSON))

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

const selectedCampaignReturnTo = computed(() => {
  const c = campaigns.value.find((x) => x.id === selectedCampaign.value)
  if (c?.returnTo?.alpha && !alpha.value) return null
  return c?.returnTo ?? null
})

const campaignScenarios = computed(() =>
  selectedCampaign.value
    ? scenarios.value.filter((s) => s.campaign == selectedCampaign.value && s.show !== false)
    : []
)

const canStandalone = computed(() => {
  if (gameMode.value !== 'Campaign') return false
  const c = campaign.value
  if (!c) return false
  return c.id !== '09'
})

const difficulties = computed<Difficulty[]>(() => {
  if (gameMode.value === 'SideStory') {
    const s = sideStories.value.find((c) => c.id === selectedScenario.value)
    if (s?.standaloneDifficulties) return s.standaloneDifficulties as Difficulty[]
    return []
  }
  return ['Easy', 'Standard', 'Hard', 'Expert']
})

const defaultCampaignName = computed(() => {
  if (gameMode.value === 'Campaign' && campaign.value) {
    const prefix = returnTo.value ? 'Return to ' : ''
    return `${prefix}${campaign.value.name}`
  }

  if (fullCampaign.value === 'Standalone' && scenario.value) {
    const prefix = returnTo.value ? 'Return to ' : ''
    return `${prefix}${scenario.value.name}`
  }

  if (gameMode.value === 'SideStory' && scenario.value) {
    return `${scenario.value.name}`
  }

  return ''
})

const currentCampaignName = computed(() => {
  return campaignName.value && campaignName.value !== ''
    ? campaignName.value
    : defaultCampaignName.value
})

const disabled = computed(() => {
  if (fullCampaign.value === 'Standalone' || gameMode.value === 'SideStory') {
    return !(scenario.value && currentCampaignName.value)
  } else {
    return !(campaign.value && currentCampaignName.value)
  }
})

// ----- step gating -----
const canGoNextFromStep1 = computed(() => {
  if (gameMode.value === 'SideStory') return !!selectedScenario.value
  return !!selectedCampaign.value
})

const nextDisabled = computed(() =>
  step.value === 'ChooseMode' ? !canGoNextFromStep1.value : disabled.value
)

// View Transition helper
function withViewTransition(fn: () => void) {
  const d = document as any
  if (typeof d.startViewTransition === 'function') {
    d.startViewTransition(() => fn())
  } else {
    fn()
  }
}

function setStep(next: Step) {
  if (step.value === next) return
  withViewTransition(() => {
    step.value = next
  })
}

function goBack() {
  if (step.value === 'GameOptions') setStep('ChooseMode')
}

async function goNext() {
  if (step.value === 'ChooseMode') {
    setStep('GameOptions')
    return
  }
  await start()
}

function cancelOrBack() {
  if (step.value === 'ChooseMode') {
    emit('close')
  } else {
    setStep('ChooseMode')
  }
}

function onKeydown(e: KeyboardEvent) {
  if (e.key !== 'Escape') return
  if (step.value !== 'ChooseMode') {
    setStep('ChooseMode') // uses view transition
  }
}

onUnmounted(() => {
  window.removeEventListener('keydown', onKeydown)
})

// ----- lifecycle / watches -----
onMounted(async () => {
  alpha.value = route.query.alpha !== undefined || localStorage.getItem('alpha') === 'true'
  if (route.query.alpha !== undefined) localStorage.setItem('alpha', 'true')
  window.addEventListener('keydown', onKeydown)
})

watch(difficulties, (ds) => {
  if (ds.length > 0) selectedDifficulty.value = ds[0]
})

watch(gameMode, (mode) => {
  // reset selections when switching mode
  returnTo.value = false
  campaignName.value = null

  if (mode === 'SideStory') {
    selectedCampaign.value = null
    fullCampaign.value = 'FullCampaign'
  } else {
    selectedScenario.value = null
  }

  step.value = 'ChooseMode'
})

watch(selectedCampaign, (id) => {
  // mirrors your selectCampaign() logic, but centralized
  selectedScenario.value = null
  returnTo.value = false
  if (id === '09') fullCampaign.value = 'FullCampaign'
})

// ----- data fetch -----
fetchDecks().then((result) => {
  decks.value = result
  ready.value = true
})

// ----- create -----
async function start() {
  if (fullCampaign.value === 'Standalone' || gameMode.value === 'SideStory') {
    if (scenario.value && currentCampaignName.value) {
      const scenarioId =
        returnTo.value && (scenario.value as any).returnTo ? (scenario.value as any).returnTo : scenario.value.id

      newGame(
        deckIds.value,
        playerCount.value,
        null,
        scenarioId,
        selectedDifficulty.value,
        currentCampaignName.value,
        multiplayerVariant.value,
        includeTarotReadings.value
      ).then((game) => router.push(`/games/${game.id}`))
    }
  } else {
    const c = campaign.value
    if (c && currentCampaignName.value) {
      const campaignId = returnTo.value && c.returnTo?.id ? c.returnTo.id : c.id

      newGame(
        deckIds.value,
        playerCount.value,
        campaignId,
        fullCampaign.value !== 'PartialCampaign' ? null : selectedScenario.value,
        selectedDifficulty.value,
        currentCampaignName.value,
        multiplayerVariant.value,
        includeTarotReadings.value
      ).then((game) => router.push(`/games/${game.id}`))
    }
  }
}

const emit = defineEmits<{
  (e: 'close'): void
}>()
</script>

<template>
  <div key="new-game" v-if="ready">
    <header class="main-header">
      <h2>{{ $t('newGame') }}</h2>
      <slot name="cancel" />
    </header>

    <form id="new-campaign" @submit.prevent="goNext">
      <ChooseMode
        v-if="step === 'ChooseMode'"
        v-model:gameMode="gameMode"
        v-model:selectedCampaign="selectedCampaign"
        v-model:selectedScenario="selectedScenario"
        :campaigns="campaigns"
        :sideStories="sideStories"
        :campaign="campaign"
        :scenario="scenario"
        @go="goNext"
      />

      <GameOptions
        v-else
        v-model:playerCount="playerCount"
        v-model:multiplayerVariant="multiplayerVariant"
        v-model:returnTo="returnTo"
        v-model:fullCampaign="fullCampaign"
        v-model:selectedScenario="selectedScenario"
        v-model:selectedDifficulty="selectedDifficulty"
        v-model:includeTarotReadings="includeTarotReadings"
        v-model:campaignName="campaignName"
        :gameMode="gameMode"
        :campaign="campaign"
        :scenario="scenario"
        :canStandalone="canStandalone"
        :selectedCampaign="selectedCampaign"
        :selectedCampaignReturnTo="selectedCampaignReturnTo"
        :campaignScenarios="campaignScenarios"
        :difficulties="difficulties"
        :currentCampaignName="currentCampaignName"
        :chosenCampaignId="selectedCampaign"
        :chosenSideStoryId="gameMode === 'SideStory' ? selectedScenario : null"
      />

      <div class="wizard-actions buttons">
        <button
          v-if="step === 'GameOptions'"
          type="button"
          class="action secondary"
          @click="goBack"
        >
          {{ $t('Back') ?? 'Back' }}
        </button>

        <button v-if="step === 'GameOptions'" class="primary-action" type="submit" :disabled="nextDisabled">
          {{ $t('create.create') }}
        </button>
      </div>
    </form>
  </div>
</template>

<style lang="css" scoped>
.container {
  min-width: 60vw;
  margin: 0 auto;
  margin-top: 20px;
}

#new-campaign {
  width: 100%;
  color: #fff;
  border-radius: 3px;
  margin-bottom: 20px;
  display: grid;
  gap: 10px;
}

#new-campaign button {
  outline: 0;
  padding: 15px;
  background: #6e8640;
  text-transform: uppercase;
  color: white;
  border: 0;
  width: 100%;
}

#new-campaign button:hover {
  background: hsl(80, 35%, 32%);
}

#new-campaign button[disabled] {
  background: #999;
  cursor: not-allowed;
}

#new-campaign button[disabled]:hover {
  background: #999;
}

#new-campaign button.secondary {
  background: hsl(80, 5%, 39%);
}

#new-campaign button.secondary:hover {
  background: hsl(80, 15%, 39%);
}

#new-campaign input[type='text'] {
  outline: 0;
  border: 1px solid var(--background);
  padding: 15px;
  background: var(--background-dark);
  width: 100%;
  margin-bottom: 10px;
}

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

.buttons {
  display: flex;
  gap: 10px;
}

.wizard-actions {
  display: grid;
  gap: 12px;
  margin-top: 6px;

  /* lay children out left-to-right */
  grid-auto-flow: column;

  /* each child becomes a column that shares space equally */
  grid-auto-columns: 1fr;

  /* no predefined columns needed */
  grid-template-columns: none;
}

/* If there is only one action, make it span full width */
.wizard-actions > *:only-child {
  grid-column: 1 / -1;
}

/* If you end up with 3+ actions, wrap to next row nicely */
.wizard-actions {
  grid-auto-flow: row;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
}

/* unify action button styling; avoid the old “full-width bar” vibe */
.wizard-actions .action {
  height: 52px;
  border-radius: 5px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(0,0,0,0.22);
  color: rgba(255,255,255,0.9);
  letter-spacing: 0.08em;
  text-transform: uppercase;
  cursor: pointer;
  box-shadow: 0 10px 22px rgba(0,0,0,0.25);
  transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease;
}

.wizard-actions .action:hover:not(:disabled) {
  transform: translateY(-1px);
  background: rgba(255,255,255,0.08);
  box-shadow: 0 5px 28px rgba(0,0,0,0.35);
}

.wizard-actions .action:active:not(:disabled) {
  transform: translateY(0px);
}

.wizard-actions .action.primary {
  background: rgba(110, 134, 64, 0.95);
  border-color: rgba(255,255,255,0.10);
  color: white;
}

.wizard-actions .action.primary:hover:not(:disabled) {
  background: rgba(110, 134, 64, 1);
}

.wizard-actions .action.secondary {
  background: rgba(255,255,255,0.06);
}

.wizard-actions .action:disabled {
  cursor: not-allowed;
  opacity: 0.55;
  box-shadow: none;
  transform: none;
}

@media (max-width: 900px) {
  .wizard-actions {
    grid-template-columns: 1fr;
  }
}

.primary-action {
  width: 100%;
  border-radius: 5px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(110, 134, 64, 0.95);
  color: white;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  box-shadow: 0 5px 28px rgba(0,0,0,0.35);
  transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease;
}

.primary-action:hover:not(:disabled) {
  transform: translateY(-1px);
  background: rgba(110, 134, 64, 1);
  box-shadow: 0 18px 34px rgba(0,0,0,0.45);
}

.primary-action:disabled {
  opacity: 0.6;
  cursor: not-allowed;
  box-shadow: none;
  transform: none;
}

header.main-header {
  view-transition-name: main-header;
  h2 {
    view-transition-name: main-header-title;
  }
  button {
    view-transition-name: main-header-button;
  }
}
</style>
