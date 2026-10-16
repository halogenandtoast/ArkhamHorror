<script lang="ts" setup>
import { LottieAnimation } from "lottie-web-vue"
import { useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import { useNarrative } from '@/arkham/composables/useNarrative'
import processingJSON from "@/assets/processing.json"
import { ref, provide, watch, computed, onMounted, onUnmounted } from 'vue'
import * as Arkham from '@/arkham/types/Game'
import { fetchGameReplay } from '@/arkham/api'
import { replayModeKey } from '@/arkham/debug'
import { isTypingTarget } from '@/arkham/helpers'
import GameLog from '@/arkham/components/GameLog.vue'
import type { LogEntry } from '@/arkham/types/GameLog'
import { logRowsToEntries } from '@/arkham/legacyLogParse'
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import Scenario from '@/arkham/components/Scenario.vue'
import Campaign from '@/arkham/components/Campaign.vue'
import { PlayIcon, StopIcon, ForwardIcon, BackwardIcon, BackspaceIcon } from '@heroicons/vue/20/solid'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const { t } = useI18n()
provide(replayModeKey, true)
provide('spectate', ref(true))
// Watching a game back is spectating it, hands included.
provide('showOtherPlayersHands', ref(true))
const ready = ref(false)
const game = ref<Arkham.Game | null>(null)

/* As in Game.vue: a narrative variant's messages follow the game being viewed. */
useNarrative(game)
const playerId = ref<string | null>(null)
const gameLog = ref<readonly LogEntry[]>(Object.freeze([]))
const totalSteps = ref(0)
const gameOver = computed(() => game.value?.gameState.tag === "IsOver")
const campaignLog = computed(() => game.value?.campaign?.log)
const recorded = computed(() => campaignLog.value?.recorded ?? [])
const recordedSets = computed<[string, unknown[]][]>(() => Object.entries(campaignLog.value?.recordedSets ?? {}))
const processing = ref(false)
const play = ref(false)
const loadError = ref(false)
provide('processing', processing)

const router = useRouter()
const currentStep = computed(() => {
  const step = Number(router.currentRoute.value.query.step ?? 0)
  return Number.isSafeInteger(step) && step >= 0 ? step : 0
})

watch([() => props.gameId, currentStep], async ([gameId, nextStep], _, onCleanup) => {
  let stale = false
  onCleanup(() => { stale = true })
  processing.value = true
  loadError.value = false
  try {
    const { game: newGame, totalSteps: newTotalSteps } = await fetchGameReplay(gameId, nextStep)
    if (stale) return
    ready.value = true
    game.value = newGame
    totalSteps.value = newTotalSteps
    gameLog.value = Object.freeze(logRowsToEntries(newGame.log, newGame.locations))
    playerId.value = newGame.activePlayerId
  } catch (error) {
    if (stale) return
    console.error('Failed to load replay step', error)
    loadError.value = true
    play.value = false
  } finally {
    if (!stale) processing.value = false
  }
}, { immediate: true })

const navigate = (step: number) => {
  if (processing.value) return
  router.replace({ query: { ...router.currentRoute.value.query, step } })
}

const handleKeyPress = (event: KeyboardEvent) => {
  if (isTypingTarget(event.target)) return
  if (!["ArrowLeft", "ArrowRight", " "].includes(event.key)) return
  event.preventDefault()
  if (event.repeat) return
  if (event.key === "ArrowLeft") goBack()
  if (event.key === "ArrowRight") goForward()
  if (event.key === " " && ready.value && !loadError.value && currentStep.value < totalSteps.value) play.value = !play.value
}

const goBack = () => {
  if (currentStep.value > 0) {
    navigate(currentStep.value - 1)
  }
}

const restart = () => {
  play.value = false
  navigate(0)
}

const goForward = () => {
  if (currentStep.value < totalSteps.value) {
    navigate(currentStep.value + 1)
  }
}

onMounted(() => document.addEventListener('keydown', handleKeyPress))
onUnmounted(() => document.removeEventListener('keydown', handleKeyPress))

const interval = ref<number | undefined>(undefined)

watch (play, (newPlay) => {
  if (newPlay) {
    interval.value = setInterval(() => {
      if(!processing.value) {
        if (currentStep.value < totalSteps.value) {
          navigate(currentStep.value + 1)
        } else {
          clearInterval(interval.value)
          play.value = false
        }
      }
    }, 2000)
  } else {
    clearInterval(interval.value)
  }
})
onUnmounted(() => clearInterval(interval.value))

</script>

<template>
  <div v-if="loadError" role="alert" class="replay-error">
    Could not load this replay step. <button @click="router.go(0)">Retry</button>
  </div>
  <div id="game" v-if="ready && game && playerId">
    <div v-if="processing" class="processing">
      <LottieAnimation 
        :animation-data="processingJSON"
        :auto-play="true"
        :loop="true"
        :speed="1"
        ref="anim" />
    </div>
    <div class="game">
      <Campaign
        v-if="game.campaign"
        :game="game"
        :campaign="game.campaign"
        :playerId="playerId"
      />
      <Scenario
        v-else-if="game.scenario && !gameOver"
        :game="game"
        :scenario="game.scenario"
        :playerId="playerId"
      />
      <div class="sidebar">
        <CardOverlay />
        <GameLog :entries="gameLog" :player-id="playerId" />
        <div class="controls" role="group" aria-label="Replay controls">
          <button v-tooltip="t('replay.restart')" :aria-label="t('replay.restart')" :disabled="processing || currentStep === 0" @click="restart"><BackspaceIcon aria-hidden="true" /></button>
          <button v-tooltip="t('replay.stepBack')" :aria-label="t('replay.stepBack')" :disabled="processing || currentStep === 0" @click="goBack"><BackwardIcon aria-hidden="true" /></button>
          <button v-tooltip="t('replay.play')" :aria-label="t('replay.play')" class="play-control" v-if="!play" :disabled="processing || loadError || currentStep >= totalSteps" @click="play = true"><PlayIcon aria-hidden="true" /></button>
          <button v-tooltip="t('replay.stop')" :aria-label="t('replay.stop')" class="play-control" v-else @click="play = false"><StopIcon aria-hidden="true" /></button>
          <button v-tooltip="t('replay.stepForward')" :aria-label="t('replay.stepForward')" :disabled="processing || currentStep >= totalSteps" @click="goForward"><ForwardIcon aria-hidden="true" /></button>
        </div>
        <div class="steps">
          <p>{{currentStep}} / {{totalSteps}}</p>
        </div>
      </div>
      <div v-if="gameOver">
        <p>{{ $t('game.gameOver') }}</p>

        <div v-for="entry in recorded" :key="entry.tag">
          {{entry}}
        </div>

        <div v-for="[key, entries] in recordedSets" :key="key">
          {{key}}: {{entries.join(", ")}}
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.action { border: 5px solid var(--select); border-radius: 15px; }

.game {
  width: 100vw;
  height: calc(100vh - var(--nav-height));
  display: flex;
}

.socketWarning  {
  backdrop-filter: blur(3px);
  background-color: rgba(0,0,0,0.8);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  display: flex;
  z-index: var(--z-index-100);

  justify-content: center;
  align-items: center;
  justify-self: center;
  align-self: center;

  p {
    padding: 10px;
    background: #FFF;
    border-radius: 4px;
  }
}

.sidebar {
  height: 100%;
  width: 25vw;
  min-width: 200px;
  flex-shrink: 0;
  min-height: 0;
  max-width: 500px;
  display: flex;
  flex-direction: column;
  background: var(--background);
}

.processing {
  z-index: var(--z-index-1000);
  position: absolute;
  top: 45px;
  left: 00px;
  width: 80px;
  filter: invert(48%) sepia(32%) saturate(393%) hue-rotate(37deg) brightness(92%) contrast(89%);
  aspect-ratio: 1;
}

.controls {
  display: flex;
  justify-content: center;
  gap: 8px;
  flex: 0 0 auto;
  padding: 14px 10px 8px;
  border-top: 1px solid var(--box-border);
}
.controls button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex: 0 0 36px;
  width: 36px;
  height: 36px;
  min-height: 0;
  padding: 0;
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: #cfd6e2;
  background: rgba(255, 255, 255, 0.07);
  cursor: pointer;
  transition: background 120ms ease, color 120ms ease;
}
.controls button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.13);
  color: #fff;
}
.controls button:focus-visible {
  outline: 2px solid var(--spooky-green);
  outline-offset: 3px;
}
.controls .play-control {
  background: var(--spooky-green);
  color: #fff;
  border-color: transparent;
}
.controls .play-control:hover:not(:disabled) {
  background: var(--spooky-green-dark);
}
.controls button:disabled { opacity: 0.4; cursor: default; }
.controls button svg { width: 20px; height: 20px; flex: none; }
.replay-error { padding: 12px; }

.steps {
  flex: none;
  padding-bottom: 12px;
  color: #cfd6e2;
  font-size: 0.8em;
  font-variant-numeric: tabular-nums;
  display: flex;
  justify-content: center;
  p {
    margin: 0;
  }
}
</style>
