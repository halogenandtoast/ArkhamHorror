<script lang="ts" setup>
import { computed, ref, watch } from 'vue'
import { BugAntIcon } from '@heroicons/vue/20/solid'
import { imgsrc } from '@/arkham/helpers'
import { chaosTokenImage, tokenOrder } from '@/arkham/types/ChaosToken'
import type { Difficulty } from '@/arkham/types/Difficulty'
import type { Scenario, Campaign } from '@/arkham/data'
import type { GameMode, MultiplayerVariant, CampaignType, AiFocus, AiSlotConfig } from '@/arkham/types/NewGame'
import { aiFocuses } from '@/arkham/types/NewGame'
import { useSettings } from '@/stores/settings'

type FullCampaignOption = {
  key: string
  difficultyLevels: Record<Difficulty, TokenFace[]>
}

type RecommendedToggle = {
  type: 'toggle'
  default?: boolean
  icon?: 'bug-ant'
  option: { tag: string }
}

const props = defineProps<{
  gameMode: GameMode
  campaign: Campaign | null | undefined
  scenario: Scenario | undefined

  canStandalone: boolean
  selectedCampaign: string | null
  selectedCampaignReturnTo: any | null

  campaignScenarios: Scenario[]
  difficulties: Difficulty[]
  currentCampaignName: string

  chosenCampaignId: string | null
  chosenSideStoryId: string | null
}>()

const playerCount = defineModel<number>('playerCount', { required: true })
const multiplayerVariant = defineModel<MultiplayerVariant>('multiplayerVariant', { required: true })
const returnTo = defineModel<boolean>('returnTo', { required: true })
const fullCampaign = defineModel<CampaignType>('fullCampaign', { required: true })
const selectedScenario = defineModel<string | null>('selectedScenario', { required: true })
const selectedDifficulty = defineModel<Difficulty>('selectedDifficulty', { required: true })
const includeTarotReadings = defineModel<boolean>('includeTarotReadings', { required: true })
const campaignName = defineModel<string | null>('campaignName', { required: true })
const fullCampaignOptionKey = defineModel<string | null>('fullCampaignOptionKey', { required: true })
const sideStoryMode = defineModel<string>('sideStoryMode', { required: true })

// "Epic Multiplayer" side-story flow. Only surfaced for side stories whose data
// carries `epicMultiplayer: true` (src/arkham/data/side-stories.json).
type EpicGroup = { name: string; playerCount: number }
const epicMode = defineModel<boolean>('epicMode', { required: true })
const epicGroupCount = defineModel<number>('epicGroupCount', { required: true })
const epicGroups = defineModel<EpicGroup[]>('epicGroups', { required: true })

// Shared time limit for the event. When the checkbox is off, NewCampaign sends 0
// minutes (= no limit / no barrier / no countdown); on, it sends `timeLimitMinutes`.
const imposeTimeLimit = defineModel<boolean>('imposeTimeLimit', { required: true })
const timeLimitMinutes = defineModel<number>('timeLimitMinutes', { required: true })

// --- AI-investigator configuration (dev-only, Solo/multihanded only) ----------
// Emits an `aiPlayers` array (length playerCount) of `AiSlotConfig | null` up to
// NewCampaign, which forwards it to newGame() only for Solo games.
const aiPlayers = defineModel<(AiSlotConfig | null)[]>('aiPlayers', { required: true })

// MVP: only Roland Banks is offered as an AI profile (single-option select).
const aiInvestigatorOptions = [{ code: '01001', name: 'Roland Banks' }]
const aiFocusOptions: Array<'auto' | AiFocus> = ['auto', ...aiFocuses]

type AiSeat = { enabled: boolean; investigator: string; focus: 'auto' | AiFocus; responseDelayMs: number }

function defaultAiSeat(): AiSeat {
  return { enabled: false, investigator: aiInvestigatorOptions[0].code, focus: 'auto', responseDelayMs: 1500 }
}

const aiSeats = ref<AiSeat[]>([])

const settings = useSettings()

// AI-investigator configuration is gated on the dev-only "AI Investigators"
// settings flag (Settings → danger zone); defaults OFF, never on in production.
const showAiConfig = computed(
  () => settings.aiInvestigatorsEnabled && multiplayerVariant.value === 'Solo' && playerCount.value > 1,
)

// Keep one seat row per player, preserving anything already configured.
watch(playerCount, (count) => {
  const next = aiSeats.value.slice(0, count)
  while (next.length < count) next.push(defaultAiSeat())
  aiSeats.value = next
}, { immediate: true })

// Project the seat rows into the `aiPlayers` model the backend expects. When AI
// config isn't applicable (non-Solo, or non-dev) we emit an empty array so a
// previously-configured Solo selection can't leak into a WithFriends game.
watch([aiSeats, showAiConfig, playerCount], () => {
  if (!showAiConfig.value) {
    aiPlayers.value = []
    return
  }
  aiPlayers.value = aiSeats.value.slice(0, playerCount.value).map((seat): AiSlotConfig | null =>
    seat.enabled
      ? {
          investigator: seat.investigator,
          focus: seat.focus === 'auto' ? undefined : seat.focus,
          responseDelayMs: seat.responseDelayMs,
        }
      : null,
  )
}, { deep: true, immediate: true })

// The epic play-mode option only appears for an epic-capable side story AND when
// the dev-only Epic Multiplayer flag is enabled (store value is dev-gated). When
// off, the side-story flow shows only the normal single-group flow.
const scenarioSupportsEpic = computed(
  () =>
    props.gameMode === 'SideStory' &&
    props.scenario?.epicMultiplayer === true &&
    settings.epicMultiplayerEnabled,
)
const isEpicActive = computed(() => scenarioSupportsEpic.value && epicMode.value)

// Keep the per-group rows in sync with the chosen group count, preserving any
// names/counts the organizer already edited.
watch(epicGroupCount, (count) => {
  const next = epicGroups.value.slice(0, count)
  while (next.length < count) next.push({ name: `Group ${String.fromCharCode(65 + next.length)}`, playerCount: 2 })
  epicGroups.value = next
})

const sideStoryScenarios = computed(() =>
  props.gameMode === 'SideStory' ? props.scenario?.scenarios ?? [] : []
)

const deckRequirements = computed(() => props.scenario?.deckRequirements ?? [])

const showAlphaWarning = computed(() => {
  if (props.gameMode === 'Campaign' && props.campaign) {
    return props.campaign.alpha
  }

  if (props.gameMode === 'SideStory' && props.scenario) {
    return props.scenario.alpha
  }

  return false
})

const showBetaWarning = computed(() => {
  if (props.gameMode === 'Campaign' && props.campaign) {
    return props.campaign.beta
  }

  if (props.gameMode === 'SideStory' && props.scenario) {
    return props.scenario.beta
  }

  return false
})

const showReturnToToggle = computed(() => {
  return (
    (props.gameMode === 'Campaign' || fullCampaign.value === 'Standalone') &&
    !!props.selectedCampaign &&
    !!props.selectedCampaignReturnTo
  )
})

const showStandaloneScenarioPicker = computed(() => {
  return (
    props.gameMode === 'Campaign' &&
    !!props.selectedCampaign &&
    (fullCampaign.value === 'Standalone' || fullCampaign.value === 'PartialCampaign')
  )
})

const selectionSummary = computed(() => {
  if (props.gameMode === 'Campaign' && props.chosenCampaignId) {
    return {
      kind: 'Campaign' as const,
      id: props.chosenCampaignId,
      title: props.campaign?.name ?? ''
    }
  }

  if (props.gameMode === 'SideStory' && props.chosenSideStoryId) {
    // title might be scenario?.name (it should exist once chosen)
    return {
      kind: 'SideStory' as const,
      id: props.chosenSideStoryId,
      title: selectedSideStoryPart.value?.name ?? props.scenario?.name ?? ''
    }
  }

  return null
})

const selectedSideStoryPart = computed(() => {
  if (props.gameMode !== 'SideStory') return null
  if (sideStoryMode.value === 'campaign') return null
  return props.scenario?.scenarios?.find((s) => s.id === sideStoryMode.value) ?? null
})

const selectionBoxSrc = computed(() => {
  if (!selectionSummary.value) return null
  const part = selectedSideStoryPart.value
  const id = part ? part.box ?? part.id : selectionSummary.value.id
  return imgsrc(`boxes/${id}.jpg`)
})

const selectionKind = computed(() => selectionSummary.value?.kind ?? null)

type TokenFace =
  | 'PlusOne' | 'Zero'
  | 'MinusOne' | 'MinusTwo' | 'MinusThree' | 'MinusFour' | 'MinusFive' | 'MinusSix' | 'MinusSeven' | 'MinusEight'
  | 'Skull' | 'Cultist' | 'Tablet' | 'ElderThing'
  | 'AutoFail' | 'ElderSign'
  | 'CurseToken' | 'BlessToken' | 'FrostToken'

function sortTokenFaces(a: TokenFace, b: TokenFace) {
  return tokenOrder.indexOf(a) - tokenOrder.indexOf(b)
}

const difficultyLevels = computed<Record<Difficulty, TokenFace[]> | null>(() => {
  const opt = selectedFullCampaignOption.value
  if (opt?.difficultyLevels) return opt.difficultyLevels

  const c = props.campaign as any
  if (c?.difficultyLevels) return c.difficultyLevels as Record<Difficulty, TokenFace[]>

  const s = props.scenario as any
  if (s?.difficultyLevels) return s.difficultyLevels as Record<Difficulty, TokenFace[]>

  return null
})

const chaosTokensForDifficulty = computed<TokenFace[]>(() => {
  const levels = difficultyLevels.value
  if (!levels) return []
  return (levels[selectedDifficulty.value] ?? []).slice().sort(sortTokenFaces)
})

const variants = computed<FullCampaignOption[]>(() => {
  const c = props.campaign as any
  return (c?.variants ?? []) as FullCampaignOption[]
})

const showFullCampaignOptions = computed(() =>
  props.gameMode === 'Campaign' &&
  fullCampaign.value === 'FullCampaign' &&
  variants.value.length > 0
)

const selectedFullCampaignOption = computed<FullCampaignOption | null>(() => {
  if (!showFullCampaignOptions.value) return null
  const key = fullCampaignOptionKey.value ?? variants.value[0]?.key
  return variants.value.find(o => o.key === key) ?? null
})

const recommendedOptionState =
  defineModel<Record<string, boolean>>('recommendedOptionState', { required: true })

const strictAsIfAt = defineModel<boolean>('strictAsIfAt', { required: true })

const rulesExpanded = ref(false)

type RulesPreset = 'chapter1' | 'chapter2'

type RulesSettings = {
  strictAsIfAt: boolean
}

const presets: Record<RulesPreset, RulesSettings> = {
  chapter1: { strictAsIfAt: false },
  chapter2: { strictAsIfAt: true },
}

const activePreset = computed<RulesPreset | null>(() => {
  for (const [key, p] of Object.entries(presets) as [RulesPreset, RulesSettings][]) {
    if (strictAsIfAt.value === p.strictAsIfAt) return key
  }
  return null
})

function applyPreset(preset: RulesPreset) {
  const p = presets[preset]
  strictAsIfAt.value = p.strictAsIfAt
}

const recommendedToggles = computed<RecommendedToggle[]>(() => {
  const c = props.campaign as any
  const opts = (c?.recommendedOptions ?? []) as RecommendedToggle[]
  return opts.filter((o) => o.type === 'toggle' && o.option?.tag)
})

function optKey(o: RecommendedToggle) {
  return o.option.tag
}

function isOptEnabled(o: RecommendedToggle) {
  const k = optKey(o)
  const v = recommendedOptionState.value[k]
  return v ?? true
}

function setOptEnabled(o: RecommendedToggle, enabled: boolean) {
  recommendedOptionState.value = {
    ...recommendedOptionState.value,
    [optKey(o)]: enabled,
  }
}
</script>

<template>
  <div class="alpha-warning" v-if="showAlphaWarning">
    {{ $t('create.alphaWarning') }}
  </div>
  <div class="beta-warning" v-if="showBetaWarning">
    {{ $t('create.betaWarning') }}
  </div>
  <div class="game-options">
    <aside v-if="selectionSummary && selectionBoxSrc" class="summary">
      <div
        class="selection-box"
        :class="{
          campaign: selectionKind === 'Campaign',
          sidestory: selectionKind === 'SideStory'
        }"
        style="view-transition-name: selected-game-box;"
      >
        <img class="selection-img" :src="selectionBoxSrc" :alt="selectionSummary.title" />

        <div class="selection-overlay">
          <div class="chip">
            {{ selectionKind === 'Campaign' ? $t('create.campaign') : $t('create.sideStory') }}
          </div>
          <div class="selection-title">{{ selectionSummary.title }}</div>
        </div>
      </div>
    </aside>

    <section class="config">
      <div v-if="deckRequirements.length" class="card deck-requirements-card">
        <div class="card-title">Deck Requirements</div>
        <ul class="deck-requirements">
          <li v-for="requirement in deckRequirements" :key="requirement">{{ requirement }}</li>
        </ul>
      </div>

      <div class="card">
        <div class="card-title">{{ $t('create.gameName') }}</div>
        <input class="text" type="text" v-model="campaignName" :placeholder="currentCampaignName" />
      </div>

      <div v-if="scenarioSupportsEpic" class="card">
        <div class="card-title">{{ $t('create.playMode') }}</div>
        <div class="segmented segmented-2">
          <input type="radio" v-model="epicMode" :value="false" id="singleGroupMode" />
          <label for="singleGroupMode">{{ $t('create.singleGroupMode') }}</label>
          <input type="radio" v-model="epicMode" :value="true" id="epicMultiplayerMode" />
          <label for="epicMultiplayerMode">{{ $t('create.epicMultiplayerMode') }}</label>
        </div>

        <transition name="slide">
          <div v-if="isEpicActive" class="subcard">
            <div class="card-title small">{{ $t('create.numberOfGroups') }}</div>
            <div class="segmented segmented-3">
              <template v-for="n in [2, 3, 4]" :key="n">
                <input type="radio" v-model="epicGroupCount" :value="n" :id="`groupCount${n}`" />
                <label :for="`groupCount${n}`">{{ n }}</label>
              </template>
            </div>

            <div class="epic-groups">
              <div v-for="(group, index) in epicGroups" :key="index" class="epic-group-row">
                <label class="epic-field">
                  <span class="card-title small">{{ $t('create.groupName') }}</span>
                  <input class="text" type="text" v-model="group.name" />
                </label>
                <label class="epic-field epic-field-count">
                  <span class="card-title small">{{ $t('create.groupPlayers') }}</span>
                  <select class="text" v-model.number="group.playerCount">
                    <option v-for="p in 4" :key="p" :value="p">{{ p }}</option>
                  </select>
                </label>
              </div>
            </div>

            <div class="epic-time-limit">
              <label class="epic-toggle">
                <input type="checkbox" v-model="imposeTimeLimit" />
                <span class="card-title small">{{ $t('create.imposeTimeLimit') }}</span>
              </label>
              <label v-if="imposeTimeLimit" class="epic-field epic-field-count">
                <span class="card-title small">{{ $t('create.timeLimitMinutes') }}</span>
                <input class="text" type="number" min="1" step="1" v-model.number="timeLimitMinutes" />
              </label>
            </div>
          </div>
        </transition>
      </div>

      <div v-if="!isEpicActive" class="card">
        <div class="card-title">{{ $t('create.numberOfPlayers') }}</div>
        <div class="segmented segmented-4">
          <input type="radio" v-model="playerCount" :value="1" id="player1" />
          <label for="player1">1</label>
          <input type="radio" v-model="playerCount" :value="2" id="player2" />
          <label for="player2">2</label>
          <input type="radio" v-model="playerCount" :value="3" id="player3" />
          <label for="player3">3</label>
          <input type="radio" v-model="playerCount" :value="4" id="player4" />
          <label for="player4">4</label>
        </div>

        <transition name="slide">
          <div v-if="playerCount > 1" class="subcard">
            <div class="card-title small">{{ $t('create.multiplayerVariant') }}</div>
            <div class="segmented segmented-2">
              <input type="radio" v-model="multiplayerVariant" value="WithFriends" id="friends" />
              <label for="friends">{{ $t('create.withFriends') }}</label>
              <input type="radio" v-model="multiplayerVariant" value="Solo" id="solo" />
              <label for="solo">{{ $t('create.multihandedSolo') }}</label>
            </div>
          </div>
        </transition>

        <transition name="slide">
        <div v-if="multiplayerVariant === 'Solo' && playerCount > 1" class="callout">
            <div class="callout-title">
              <font-awesome-icon icon="eye" class="callout-icon" />
              {{ $t('create.switchingPerspectives') }}
            </div>
            <div class="callout-body" v-html="$t('create.switchingPerspectivesDescription')"></div>
          </div>
        </transition>

        <transition name="slide">
          <div v-if="showAiConfig" class="subcard ai-config">
            <div class="card-title small ai-config-title">
              AI Investigators <span class="ai-dev-pill">dev</span>
            </div>
            <div class="ai-seats">
              <div v-for="(seat, index) in aiSeats.slice(0, playerCount)" :key="index" class="ai-seat">
                <label class="ai-seat-toggle">
                  <input type="checkbox" v-model="seat.enabled" />
                  <span>Seat {{ index + 1 }} — AI controlled</span>
                </label>

                <transition name="slide">
                  <div v-if="seat.enabled" class="ai-seat-fields">
                    <label class="ai-field">
                      <span class="card-title small">Investigator</span>
                      <select class="text" v-model="seat.investigator">
                        <option v-for="inv in aiInvestigatorOptions" :key="inv.code" :value="inv.code">
                          {{ inv.name }}
                        </option>
                      </select>
                    </label>
                    <label class="ai-field">
                      <span class="card-title small">Focus</span>
                      <select class="text" v-model="seat.focus">
                        <option v-for="focus in aiFocusOptions" :key="focus" :value="focus">{{ focus }}</option>
                      </select>
                    </label>
                    <label class="ai-field">
                      <span class="card-title small">Response delay (ms)</span>
                      <input class="text" type="number" min="0" step="100" v-model.number="seat.responseDelayMs" />
                    </label>
                  </div>
                </transition>
              </div>
            </div>
          </div>
        </transition>
      </div>

      <div v-if="sideStoryScenarios.length > 0" class="card">
        <div class="card-title">{{ $t('create.scenarios') }}</div>
        <div class="segmented" :class="`segmented-${sideStoryScenarios.length + 1}`">
          <input type="radio" v-model="sideStoryMode" value="campaign" id="sideStoryBoth" />
          <label for="sideStoryBoth">{{ $t('create.bothScenarios') }}</label>

          <template v-for="s in sideStoryScenarios" :key="s.id">
            <input type="radio" v-model="sideStoryMode" :value="s.id" :id="`sideStoryPart-${s.id}`" />
            <label :for="`sideStoryPart-${s.id}`">{{ s.name }}</label>
          </template>
        </div>
      </div>

      <div v-if="showReturnToToggle" class="card">
        <div class="card-title">{{ $t('create.returnTo') }}</div>
        <div class="segmented segmented-2">
          <input type="radio" v-model="returnTo" :value="false" id="normal" />
          <label for="normal">{{ $t('create.normal') }}</label>
          <input type="radio" v-model="returnTo" :value="true" id="returnTo" />
          <label for="returnTo">{{ $t('create.returnTo') }}</label>
        </div>
      </div>

      <div v-if="canStandalone" class="card">
        <div class="card-title">{{ $t('create.campaignType') }}</div>
        <div class="segmented segmented-3">
          <input type="radio" v-model="fullCampaign" :value="'FullCampaign'" id="full" />
          <label for="full">{{ $t('create.fullCampaign') }}</label>

          <input type="radio" v-model="fullCampaign" :value="'Standalone'" id="standalone" />
          <label for="standalone">{{ $t('create.standalone') }}</label>

          <template v-if="campaign?.settings">
            <input type="radio" v-model="fullCampaign" :value="'PartialCampaign'" id="partial" />
            <label for="partial">{{ $t('create.partialCampaign') }}</label>
          </template>
        </div>

        <template v-if="showStandaloneScenarioPicker">
          <div class="subcard">
            <div class="card-title small">{{ $t('create.pickScenario') }}</div>
            <div class="scenario-grid">
              <button
                v-for="s in campaignScenarios"
                :key="s.id"
                type="button"
                class="scenario-tile"
                :class="{ selected: selectedScenario == s.id }"
                @click="selectedScenario = s.id"
              >
                <img :src="imgsrc(`boxes/${s.id}.jpg`)" :alt="s.name" />
              </button>
            </div>
          </div>
        </template>
      </div>

      <div v-if="showFullCampaignOptions" class="card">
        <div class="card-title">{{ $t('create.variant') }}</div>

        <div class="segmented" :class="`segmented-${variants.length}`">
          <template v-for="opt in variants" :key="opt.key">
            <input
              type="radio"
              v-model="fullCampaignOptionKey"
              :value="opt.key"
              :id="`fullCampaignOption-${opt.key}`"
            />
            <label class="variant" :for="`fullCampaignOption-${opt.key}`" v-html="$t(`create.fullCampaignOption.${opt.key}`)" />
          </template>
        </div>
      </div>

      <div class="card">
        <div class="card-title">{{ $t('create.difficulty') }}</div>
        <div class="segmented segmented-4">
          <template v-for="difficulty in difficulties" :key="difficulty">
            <input
              type="radio"
              v-model="selectedDifficulty"
              :value="difficulty"
              :id="`difficulty${difficulty}`"
            />
            <label :for="`difficulty${difficulty}`">{{ $t('create.' + difficulty) }}</label>
          </template>
        </div>
        <div v-if="chaosTokensForDifficulty.length > 0" class="token-preview callout">
          <img
            v-for="(tokenFace, idx) in chaosTokensForDifficulty"
            :key="`${tokenFace}-${idx}`"
            class="token"
            :src="chaosTokenImage(tokenFace)"
            :alt="tokenFace"
          />
        </div>
      </div>

      <div class="card">
        <div class="card-title">{{ $t('create.includeTarotReadings') }}</div>
        <div class="segmented segmented-2">
          <input type="radio" v-model="includeTarotReadings" :value="false" id="tarotNo" />
          <label for="tarotNo">{{ $t('No') }}</label>

          <input type="radio" v-model="includeTarotReadings" :value="true" id="tarotYes" />
          <label for="tarotYes">{{ $t('Yes') }}</label>
        </div>
      </div>

      <div class="card rules-card">
        <button type="button" class="rules-toggle" @click="rulesExpanded = !rulesExpanded">
          <span class="card-title" style="margin-bottom: 0">{{ $t('create.advancedRulesConfiguration') ?? 'Advanced Rules Configuration' }}</span>
          <span class="rules-header-right">
            <span class="preset-pill" :class="activePreset ?? 'custom'">
              {{ activePreset ? ($t(`create.preset.${activePreset}.name`) ?? activePreset) : ($t('create.presetCustom') ?? 'Custom') }}
            </span>
            <span class="rules-chevron" :class="{ expanded: rulesExpanded }">▸</span>
          </span>
        </button>
        <transition name="slide">
          <div v-if="rulesExpanded" class="rules-body subcard">
            <div class="card-title small">{{ $t('create.rulesPresets') ?? 'Presets' }}</div>
            <div class="preset-options">
              <button
                v-for="preset in (['chapter1', 'chapter2'] as RulesPreset[])"
                :key="preset"
                type="button"
                class="preset-option"
                :class="{ selected: activePreset === preset }"
                @click="applyPreset(preset)"
              >
                <span class="preset-name">{{ $t(`create.preset.${preset}.name`) ?? preset }}</span>
                <span class="preset-desc">{{ $t(`create.preset.${preset}.description`) ?? '' }}</span>
              </button>
            </div>

            <div class="rule-setting subcard">
              <div class="card-title small">{{ $t('create.asIfAtBehavior') ?? '"As If" At Behavior' }}</div>
              <div class="as-if-at-options">
                <label class="as-if-at-option" :class="{ selected: !strictAsIfAt }" @click="strictAsIfAt = false">
                  <div class="as-if-at-header">
                    <input type="radio" v-model="strictAsIfAt" :value="false" id="asIfAtChapter1" />
                    <span class="as-if-at-name">{{ $t('create.asIfAtChapter1') ?? 'Chapter 1 Rules' }}</span>
                  </div>
                  <div class="as-if-at-desc">{{ $t('create.asIfAtChapter1Description') ?? '' }}</div>
                </label>
                <label class="as-if-at-option" :class="{ selected: strictAsIfAt }" @click="strictAsIfAt = true">
                  <div class="as-if-at-header">
                    <input type="radio" v-model="strictAsIfAt" :value="true" id="asIfAtChapter2" />
                    <span class="as-if-at-name">{{ $t('create.asIfAtChapter2') ?? 'Chapter 2 Rules' }}</span>
                  </div>
                  <div class="as-if-at-desc">{{ $t('create.asIfAtChapter2Description') ?? '' }}</div>
                </label>
              </div>
            </div>
          </div>
        </transition>
      </div>

      <div v-if="recommendedToggles.length > 0" class="card">
        <div class="card-title">{{ $t('create.recommendedOptions') ?? 'Recommended options' }}</div>

        <div class="recommended-list">
          <div class="recommended-row" v-for="o in recommendedToggles" :key="optKey(o)">
            <div class="recommended-text">
              <div class="recommended-name">
                <BugAntIcon v-if="o.icon === 'bug-ant'" class="recommended-icon" aria-hidden="true" />
                {{ $t(`create.recommendedOption.${o.option.tag}.title`) ?? o.option.tag }}
              </div>
              <div class="recommended-desc" v-if="$te?.(`create.recommendedOption.${o.option.tag}.description`)">
                {{ $t(`create.recommendedOption.${o.option.tag}.description`) }}
              </div>
            </div>

            <div class="segmented segmented-2 recommended-toggle">
              <input
                type="radio"
                :id="`rec-${optKey(o)}-on`"
                :checked="isOptEnabled(o)"
                @change="setOptEnabled(o, true)"
              />
              <label :for="`rec-${optKey(o)}-on`">{{ $t('On') ?? 'On' }}</label>

              <input
                type="radio"
                :id="`rec-${optKey(o)}-off`"
                :checked="!isOptEnabled(o)"
                @change="setOptEnabled(o, false)"
              />
              <label :for="`rec-${optKey(o)}-off`">{{ $t('Off') ?? 'Off' }}</label>
            </div>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<style lang="scss" scoped>
.game-options {
  display: grid;
  grid-template-columns: 360px minmax(0, 1fr);
  gap: 16px;
  align-items: start;
}

@media (max-width: 1100px) {
  .game-options {
    grid-template-columns: 1fr;
  }
}

.summary {
  position: sticky;
}

.config {
  min-width: 0;
  display: grid;
  gap: 12px;
}

.selection-box.campaign {
  aspect-ratio: 4 / 3;
}

.selection-box.sidestory {
  aspect-ratio: 3 / 2;
}

.selection-img {
  inset: 0;
  width: 100%;
  border-radius: 14px;
  display: block;
  object-fit: cover;
  object-position: 50% 50%;
  filter: contrast(1.05);
  outline: 1px solid rgba(154 196 78 / 0.55);
  pointer-events: none;
}

.selection-overlay {
  border-radius: 14px;
  position: absolute;
  inset: 0;
  display: flex;
  flex-direction: column;
  justify-content: flex-end;
  padding: 10px;
  background: linear-gradient(
    to top,
    rgba(0 0 0 / 0.85),
    rgba(0 0 0 / 0.05) 60%,
    rgba(0 0 0 / 0)
  );
  pointer-events: none;
}

.chip {
  align-self: flex-start;
  display: inline-flex;
  align-items: center;
  height: 22px;
  padding: 0 10px;
  border-radius: 999px;
  font-size: 12px;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  background: rgba(255, 255, 255, 0.14);
  border: 1px solid rgba(255, 255, 255, 0.18);
  backdrop-filter: blur(6px);
  margin-bottom: 6px;
}

.selection-title {
  font-family: Teutonic;
  font-size: 18px;
  letter-spacing: 0.02em;
  text-shadow: 0 2px 10px rgba(0, 0, 0, 0.6);
}

/* Cards */
.card {
  border-radius: 12px;
  background: rgba(0, 0, 0, 0.18);
  border: 1px solid rgba(255, 255, 255, 0.06);
  padding: 12px;
  box-shadow: 0 8px 22px rgba(0, 0, 0, 0.22);
}

.subcard {
  margin-top: 10px;
  padding-top: 10px;
  border-top: 1px solid rgba(255, 255, 255, 0.08);
}

.card-title {
  font-size: 12px;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.78);
  margin-bottom: 8px;
}

.card-title.small {
  opacity: 0.85;
}

.deck-requirements-card {
  border-color: rgba(255, 211, 112, 0.18);
  background: rgba(95, 65, 10, 0.24);
}

.deck-requirements {
  margin: 0;
  padding-left: 20px;
  color: rgba(255, 226, 154, 0.95);
  line-height: 1.35;
  font-size: 13px;
}

/* Text input */
.text {
  width: 100%;
  outline: 0;
  border: 1px solid rgba(255, 255, 255, 0.10);
  border-radius: 10px;
  padding: 10px 12px;
  background: rgba(0, 0, 0, 0.22);
  color: #fff;
}

.text::placeholder {
  color: rgba(255, 255, 255, 0.45);
}

/* Segmented controls */
input[type='radio'] {
  display: none;
}

.segmented {
  display: grid;
  border-radius: 12px;
  overflow: hidden;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(0, 0, 0, 0.12);
}

.segmented-2 { grid-template-columns: repeat(2, 1fr); }
.segmented-3 { grid-template-columns: repeat(3, 1fr); }
.segmented-4 { grid-template-columns: repeat(4, 1fr); }

.segmented label {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 12px;
  user-select: none;
  cursor: pointer;
  background: rgba(255, 255, 255, 0.06);
  border-right: 1px solid rgba(255, 255, 255, 0.08);
}

.segmented label:last-of-type {
  border-right: none;
}

.segmented label:hover {
  background: rgba(255, 255, 255, 0.10);
}

input[type='radio']:checked + label {
  background: rgba(110, 134, 64, 0.95);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.10);
}

/* Scenario picker */
.scenario-grid {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 10px;
}

@media (max-width: 1100px) {
  .scenario-grid {
    grid-template-columns: repeat(3, 1fr);
  }
}

.scenario-tile {
  border: 0;
  padding: 0;
  background: transparent;
  border-radius: 12px;
  overflow: hidden;
  cursor: pointer;
  box-shadow: 0 10px 24px rgba(0, 0, 0, 0.35);
  outline: 1px solid rgba(255, 255, 255, 0.08);
}

.scenario-tile img {
  width: 100%;
  display: block;
  filter: grayscale(100%);
  transition: filter 220ms ease, transform 220ms ease;
}

.scenario-tile:hover img {
  filter: grayscale(100%) sepia(1);
  transform: scale(1.02);
}

.scenario-tile.selected img {
  filter: none;
}

/* Slide transition */
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

.callout {
  margin-top: 10px;
  padding: 12px;
  border-radius: 12px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(0,0,0,0.18);
  box-shadow: 0 10px 22px rgba(0,0,0,0.22);
}

.callout-title {
  display: flex;
  align-items: center;
  gap: 10px;
  font-size: 12px;
  letter-spacing: 0.10em;
  text-transform: uppercase;
  color: rgba(255,255,255,0.82);
  margin-bottom: 6px;
}

.callout-icon {
  opacity: 0.9;
  animation: glow 1.5s infinite alternate;
}

.callout-body {
  font-size: 13px;
  line-height: 1.35;
  color: rgba(255,255,255,0.72);
}

@keyframes glow {
  from {
    color: #000;
    text-shadow: 0 0 0px var(--select);
  }
  to {
    color: var(--select); /* Glowing color */
    text-shadow: 0 0 10px var(--select);
  }
}

.token-preview {
  display: flex;
  gap: 5px;
  flex-wrap: wrap;
  flex-direction: row;
  justify-content: center;
  @media (max-width: 800px) and (orientation: portrait) {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr 3fr 1fr 1fr 1fr;
    img{
      margin: 0 auto;
    }
    img:nth-child(6n+1) {
      grid-column: 1;
    }

    img:nth-child(6n+2) {
      grid-column: 2;
    }

    img:nth-child(6n+3) {
      grid-column: 3;
    }

    img:nth-child(6n+4) {
      grid-column: 5;
    }

    img:nth-child(6n+5) {
      grid-column: 6;
    }

    img:nth-child(6n) {
      grid-column: 7;
    }
  }
  
  img {
    width: 30px;
    height: auto;
    transition: transform 0.2s;
    &:hover {
      transform: scale(1.2);
    }
    &.token-big {
      width: 50px;
      border-radius: 50px;
    }
    border: 1px solid rgba(255,255,255,0.4);
    border-radius: 30px;
    box-shadow: 0 4px 4px rgba(0,0,0,0.5);
  }
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

.variant {
  display: flex;
  flex-direction: column;
  :deep(small) {
    font-size: 0.85em;
    margin-top: 4px;
    color: rgba(255 255 255 / 0.75);
  }
}

.epic-groups {
  margin-top: 10px;
  display: grid;
  gap: 10px;
}

.epic-group-row {
  display: grid;
  grid-template-columns: 1fr 120px;
  gap: 10px;
}

.epic-field {
  display: grid;
  gap: 4px;
}

.epic-field .text {
  border-radius: 10px;
}

.epic-time-limit {
  margin-top: 14px;
  display: flex;
  align-items: flex-end;
  gap: 16px;
  flex-wrap: wrap;
}

.epic-toggle {
  display: flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
}

.epic-toggle input[type='checkbox'] {
  width: 18px;
  height: 18px;
  cursor: pointer;
}

.epic-time-limit .epic-field-count {
  width: 120px;
}

.ai-config-title {
  display: flex;
  align-items: center;
  gap: 8px;
}

.ai-dev-pill {
  font-size: 10px;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  padding: 1px 6px;
  border-radius: 999px;
  border: 1px solid rgba(184, 134, 11, 0.55);
  background: rgba(184, 134, 11, 0.25);
  color: rgba(255, 226, 154, 0.95);
}

.ai-seats {
  display: grid;
  gap: 10px;
}

.ai-seat {
  padding: 10px;
  border-radius: 10px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(0, 0, 0, 0.12);
}

.ai-seat-toggle {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  color: rgba(255, 255, 255, 0.85);
  cursor: pointer;
}

.ai-seat-toggle input[type='checkbox'] {
  width: 15px;
  height: 15px;
  accent-color: rgb(110, 134, 64);
}

.ai-seat-fields {
  margin-top: 10px;
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 10px;
}

@media (max-width: 700px) {
  .ai-seat-fields {
    grid-template-columns: 1fr;
  }
}

.ai-field {
  display: grid;
  gap: 4px;
}

.ai-field select.text,
.ai-field input.text {
  text-transform: capitalize;
}

.recommended-list {
  display: grid;
  gap: 10px;
}

.recommended-row {
  display: grid;
  grid-template-columns: 1fr auto;
  gap: 12px;
  align-items: center;
  padding-top: 8px;
  border-top: 1px solid rgba(255,255,255,0.08);
}

.recommended-row:first-child {
  border-top: none;
  padding-top: 0;
}

.recommended-name {
  font-size: 13px;
  color: rgba(255,255,255,0.85);
}

.recommended-desc {
  margin-top: 4px;
  font-size: 12px;
  line-height: 1.3;
  color: rgba(255,255,255,0.65);
}

.recommended-toggle {
  width: 180px;
}

.rules-body {
  display: grid;
  gap: 12px;
}

.preset-options {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px;
}

.preset-option {
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 10px 12px;
  border-radius: 10px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(0, 0, 0, 0.12);
  color: inherit;
  cursor: pointer;
  text-align: left;
  transition: background 150ms ease, border-color 150ms ease;
}

.preset-option.selected {
  background: rgba(110, 134, 64, 0.2);
  border-color: rgba(110, 134, 64, 0.6);
}

.preset-option:hover:not(.selected) {
  background: rgba(255, 255, 255, 0.06);
}

.preset-name {
  font-size: 13px;
  font-weight: 500;
  color: rgba(255, 255, 255, 0.9);
}

.preset-desc {
  font-size: 11px;
  line-height: 1.3;
  color: rgba(255, 255, 255, 0.55);
}

.rule-setting {
  margin-top: 0;
}

.as-if-at-options {
  display: grid;
  gap: 8px;
}

.as-if-at-option {
  padding: 10px 12px;
  border-radius: 10px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(0, 0, 0, 0.12);
  cursor: pointer;
  transition: background 150ms ease, border-color 150ms ease;
}

.as-if-at-option.selected {
  background: rgba(110, 134, 64, 0.2);
  border-color: rgba(110, 134, 64, 0.6);
}

.as-if-at-header {
  display: flex;
  align-items: center;
  gap: 8px;
}

.as-if-at-header :deep(input[type='radio']),
.as-if-at-header input[type='radio'] {
  display: inline-block !important;
  width: 14px;
  height: 14px;
  flex-shrink: 0;
  accent-color: rgb(110, 134, 64);
}

.as-if-at-name {
  font-size: 13px;
  color: rgba(255, 255, 255, 0.9);
  font-weight: 500;
}

.as-if-at-desc {
  margin-top: 5px;
  font-size: 12px;
  line-height: 1.35;
  color: rgba(255, 255, 255, 0.6);
}

.rules-header-right {
  display: flex;
  align-items: center;
  gap: 8px;
}

.preset-pill {
  font-size: 11px;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  padding: 2px 8px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.15);
  background: rgba(255, 255, 255, 0.08);
  color: rgba(255, 255, 255, 0.6);
}

.preset-pill.chapter1,
.preset-pill.chapter2 {
  background: rgba(110, 134, 64, 0.25);
  border-color: rgba(110, 134, 64, 0.55);
  color: rgba(180, 210, 120, 0.9);
}

.rules-toggle {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  padding: 0;
}

.rules-chevron {
  font-size: 12px;
  color: rgba(255, 255, 255, 0.55);
  transition: transform 200ms ease;
}

.rules-chevron.expanded {
  transform: rotate(90deg);
}

.recommended-icon {
  width: 1em;
  height: 1em;
  vertical-align: -0.15em;
  margin-right: 0.4em;
  opacity: 0.75;
}

@media (max-width: 700px) {
  .recommended-row {
    grid-template-columns: 1fr;
  }
  .recommended-toggle {
    width: 100%;
  }
}
</style>
