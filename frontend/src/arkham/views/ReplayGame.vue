<script lang="ts" setup>
import { LottieAnimation } from "lottie-web-vue"
import { useRouter } from 'vue-router'
import processingJSON from "@/assets/processing.json"
import { ref, provide, watch, computed, onMounted, onUnmounted } from 'vue'
import * as Arkham from '@/arkham/types/Game'
import { fetchGameReplay } from '@/arkham/api'
import GameLog from '@/arkham/components/GameLog.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import Scenario from '@/arkham/components/Scenario.vue'
import Campaign from '@/arkham/components/Campaign.vue'
import { PlayIcon, StopIcon, ForwardIcon, BackwardIcon, BackspaceIcon } from '@heroicons/vue/20/solid'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const debug = ref(false)
provide('debug', debug)
const ready = ref(false)
const game = ref<Arkham.Game | null>(null)
const playerId = ref<string | null>(null)
const gameLog = ref<readonly string[]>(Object.freeze([]))
const totalSteps = ref(0)
const gameOver = computed(() => game.value?.gameState.tag === "IsOver")
const campaignLog = computed(() => game.value?.campaign?.log)
const recorded = computed(() => campaignLog.value?.recorded ?? [])
const recordedSets = computed(() => campaignLog.value?.recordedSets ?? [])
const processing = ref(false)
const play = ref(false)

const router = useRouter()
const currentStep = computed(() => parseInt(router.currentRoute.value.query.step as string) || 0)


watch(currentStep, nextStep => {
  processing.value = true;
  fetchGameReplay(props.gameId, nextStep).then(({ game: newGame, totalSteps: newTotalSteps }) => {
    ready.value = true;
    game.value = newGame;
    totalSteps.value = newTotalSteps
    gameLog.value = Object.freeze(newGame.log);
    playerId.value = newGame.activePlayerId;
    processing.value = false;
  });
}, {immediate: true})

const handleKeyPress = (event: KeyboardEvent) => {
  if (event.key === "ArrowLeft") goBack()
  if (event.key === "ArrowRight") goForward()
  if (event.key === " ") play.value = !play.value
}

const goBack = () => {
  if (currentStep.value > 0) {
    router.push({ query: { step: currentStep.value - 1 } })
  }
}

const restart = () => {
  router.push({ query: { step: 0 } })
}

const goForward = () => {
  if (currentStep.value < totalSteps.value) {
    router.push({ query: { step: currentStep.value + 1 } })
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
          router.push({ query: { step: currentStep.value + 1 } })
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

</script>

<template>
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
        :gameLog="gameLog"
        :playerId="playerId"
      />
      <Scenario
        v-else-if="game.scenario && !gameOver"
        :game="game"
        :gameLog="gameLog"
        :scenario="game.scenario"
        :playerId="playerId"
      />
      <div class="sidebar">
        <CardOverlay />
        <GameLog :game="game" :gameLog="gameLog" />
        <div class="controls">
          <button v-tooltip="'Restart'" :disabled="processing" @click="restart"><BackspaceIcon size="25" /></button>
          <button v-tooltip="'Step back'" :disabled="processing" @click="goBack"><BackwardIcon size="25" /></button>
          <button v-tooltip="'Play'" v-if="!play" @click="play = true"><PlayIcon size="25" /></button>
          <button v-tooltip="'Stop'" v-else @click="play = false"><StopIcon size="25" /></button>
          <button v-tooltip="'Step forward'" :disabled="processing" @click="goForward"><ForwardIcon size="25" /></button>
        </div>
        <div class="steps">
          <p>{{currentStep}} / {{totalSteps}}</p>
        </div>
      </div>
      <div v-if="gameOver">
        <p>Game over</p>

        <div v-for="entry in recorded" :key="entry">
          {{entry}}
        </div>

        <div v-for="(entry, idx) in recordedSets" :key="idx">
          {{(entry as any[])[0]}}: {{(entry as any[])[1].join(", ")}}
        </div>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.action { border: 5px solid var(--select); border-radius: 15px; }

.game {
  width: 100vw;
  height: calc(100vh - 40px);
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
  z-index: 100;

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
  max-width: 500px;
  display: flex;
  flex-direction: column;
  background: #d0d9dc;
}

.processing {
  z-index: 1000;
  position: absolute;
  top: 45px;
  left: 00px;
  width: 80px;
  filter: invert(48%) sepia(32%) saturate(393%) hue-rotate(37deg) brightness(92%) contrast(89%);
  aspect-ratio: 1;
}

.controls {
  display: flex;
  gap: 2px;
  button {
    flex: 1;
    width: 30px;
    height: 30px;
    :deep(svg) {
      width: 25px;
    }
  }
  height: 30px;
  margin-inline: 10px;
}

.steps {
  display: flex;
  justify-content: center;
  p {
    margin: 0;
  }
}
</style>
