<script lang="ts" setup>
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'
import { chaosTokenImage, tokenOrder } from '@/arkham/types/ChaosToken'
import type { Difficulty } from '@/arkham/types/Difficulty'
import type { Scenario, Campaign } from '@/arkham/data'
import type { GameMode, MultiplayerVariant, CampaignType } from '@/arkham/types/NewGame'

type FullCampaignOption = {
  key: string
  difficultyLevels: Record<Difficulty, TokenFace[]>
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
      title: props.scenario?.name ?? ''
    }
  }

  return null
})

const selectionBoxSrc = computed(() =>
  selectionSummary.value ? imgsrc(`boxes/${selectionSummary.value.id}.jpg`) : null
)

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
      <div class="card">
        <div class="card-title">{{ $t('create.gameName') }}</div>
        <input class="text" type="text" v-model="campaignName" :placeholder="currentCampaignName" />
      </div>

      <div class="card">
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
    color: #000; /* Or any other default color */
    text-shadow: 0 0 0px #ff00ff;
  }
  to {
    color: #ff00ff; /* Glowing color */
    text-shadow: 0 0 10px #ff00ff;
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
</style>
