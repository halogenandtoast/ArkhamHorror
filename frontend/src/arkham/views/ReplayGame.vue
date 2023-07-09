<script lang="ts" setup>
import { ref, provide, watch, computed } from 'vue'
import * as Arkham from '@/arkham/types/Game'
import { fetchGameReplay } from '@/arkham/api'
import GameLog from '@/arkham/components/GameLog.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import Scenario from '@/arkham/components/Scenario.vue'
import Campaign from '@/arkham/components/Campaign.vue'

export interface Props {
  gameId: string
}

const props = defineProps<Props>()
const debug = ref(false)
provide('debug', debug)
const ready = ref(false)
const game = ref<Arkham.Game | null>(null)
const investigatorId = ref<string | null>(null)
const gameLog = ref<readonly string[]>(Object.freeze([]))
const step = ref(1)
const totalSteps = ref(0)
const gameOver = computed(() => props.game.gameState.tag === "IsOver")

watch(step, currentStep => {
  fetchGameReplay(props.gameId, currentStep).then(({ game: newGame, totalSteps: newTotalSteps }) => {
    ready.value = true;
    game.value = newGame;
    totalSteps.value = newTotalSteps
    gameLog.value = Object.freeze(newGame.log);
    investigatorId.value = newGame.activeInvestigatorId;
  });
}, {immediate: true})
</script>

<template>
  <div id="game" v-if="ready">
    <div class="game">
      <Campaign
        v-if="game.campaign"
        :game="game"
        :gameLog="gameLog"
        :investigatorId="investigatorId"
      />
      <Scenario
        v-else-if="game.scenario && !gameOver"
        :game="game"
        :gameLog="gameLog"
        :scenario="game.scenario"
        :investigatorId="investigatorId"
      />
      <div class="sidebar">
        <CardOverlay />
        <GameLog :game="game" :gameLog="gameLog" />
        <button @click="step = 0">Start</button>
        <button @click="step -= 1">Previous</button>
        <button @click="step += 1">Next</button>
      </div>
      <div v-if="game.gameOver">
        <p>Game over</p>

        <div v-for="entry in game.campaign.log.recorded" :key="entry">
          {{entry}}
        </div>

        <div v-for="(entry, idx) in game.campaign.log.recordedSets" :key="idx">
          {{entry[0]}}: {{entry[1].join(", ")}}
        </div>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.action { border: 5px solid $select; border-radius: 15px; }

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
  box-sizing: border-box;
}
</style>
