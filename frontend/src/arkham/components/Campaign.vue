<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Campaign } from '@/arkham/types/Campaign';
import StoryQuestion from '@/arkham/components/StoryQuestion.vue';
import Scenario from '@/arkham/components/Scenario.vue';
import UpgradeDeck from '@/arkham/components/UpgradeDeck.vue';
import ChooseDeck from '@/arkham/components/ChooseDeck.vue';
import ContinueCampaign from '@/arkham/components/ContinueCampaign.vue';

const props = defineProps<{
  game: Game
  campaign: Campaign
  playerId: string
}>()

const emit = defineEmits<{
  update: [game: Game]
  choose: [idx: number]
}>()

async function update(game: Game) {
  emit('update', game);
}

async function choose(idx: number) {
  emit('choose', idx)
}

const chooseDeck = computed(() => {
  if (props.game.campaign && props.game.campaign.step?.tag === 'ChooseDecksStep') return true
  const question = Object.values(props.game.question)[0]

  if (question === null || question == undefined) {
    return false
  }

  const { tag } = question

  if (tag === 'ChooseDeck' || props.game.gameState.tag === 'IsChooseDecks') {
    return true
  }

  if (tag === 'QuestionLabel') {
    return question.question.tag === 'ChooseDeck'
  }

  return false
})


const questionLabel = computed(() => {
  let question = props.game.question[props.playerId]

  if (!question && chooseDeck.value) {
    question = Object.values(props.game.question)[0]
  }

  if (!question) return null

  return question.tag === 'QuestionLabel' ? question.label : null
})

const continueCampaign = computed(() => {
  if (props.game.campaign && props.game.campaign.step?.tag === 'ContinueCampaignStep') return props.game.campaign.step.contents
  return null
})

const upgradeDeck = computed(() => {
  if (props.game.campaign && props.game.campaign.step?.tag === 'UpgradeDeckStep') return true

  const question = Object.values(props.game.question)[0]

  if (question === null || question == undefined) {
    return false
  }

  const { tag } = question

  if (tag === 'ChooseUpgradeDeck' && props.game.gameState.tag === 'IsChooseDecks') {
    return true
  }

  if (tag === 'QuestionLabel') {
    return question.question.tag === 'ChooseUpgradeDeck'
  }

  return false
})

const pickDestiny = computed(() => {
  const question = Object.values(props.game.question)[0]

  if (question === null || question == undefined) {
    return false
  }

  const { tag } = question

  if (tag === 'PickDestiny') {
    return true
  }

  return false
})

const questionHash = computed(() => {
  let question = JSON.stringify(props.game.question[props.playerId])
  return btoa(encodeURIComponent(question))
})

const continueScenario = computed(() => {
  if (props.game.scenario?.campaignStep?.tag === 'ContinueCampaignStep') return props.game.scenario?.campaignStep.contents
  return null
})

const inScenarioStep = computed(() => {
  return !!props.game.scenario?.campaignStep
})
</script>

<template>
  <div v-if="upgradeDeck" id="game" class="game">
    <UpgradeDeck :game="game" :playerId="playerId" @choose="choose" />
  </div>
  <div v-else-if="chooseDeck" id="game" class="game">
    <h2 v-if="questionLabel" class="title question-label">{{ questionLabel }}</h2>
    <ChooseDeck :game="game" :playerId="playerId" @choose="choose" />
  </div>
  <div v-else-if="continueCampaign" id="game" class="game">
    <ContinueCampaign
      :game="game"
      :campaign="campaign"
      :playerId="playerId"
      :canUpgradeDecks="continueCampaign.canUpgradeDecks"
      :step="continueCampaign.nextStep"
      :chooseSideStory="continueCampaign.chooseSideStory"
      :canChooseSideStory="continueCampaign.canChooseSideStory"
      @choose="choose"
    />
  </div>
  <div v-else-if="game.gameState.tag === 'IsActive'" id="game" class="game">
    <template v-if="pickDestiny">
      <StoryQuestion :game="game" :key="questionHash" :playerId="playerId" @choose="choose" />
    </template>
    <ContinueCampaign
      v-if="continueScenario"
      :game="game"
      :scenario="game.scenario ?? undefined"
      :playerId="playerId"
      :canUpgradeDecks="continueScenario.canUpgradeDecks"
      :step="continueScenario.nextStep"
      :chooseSideStory="continueScenario.chooseSideStory"
      :canChooseSideStory="continueScenario.canChooseSideStory"
      @choose="choose"
    />
    <Scenario
      v-else-if="game.scenario && game.scenario.started && Object.entries(game.investigators).length > 0 && !inScenarioStep"
      :game="game"
      :scenario="game.scenario"
      :playerId="playerId"
      @choose="choose"
      @update="update"
    />
    <template v-else>
      <StoryQuestion :game="game" :key="questionHash" :playerId="playerId" @choose="choose" />
    </template>
  </div>
</template>

<style scoped>
.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: var(--card-width);
}

.card--sideways {
  width: auto;
  height: calc(var(--card-width) * 2);
}

.scenario-cards {
  display: flex;
  align-self: center;
  align-items: flex-start;
  justify-content: center;
  padding-bottom: 10px;
}

.clue--can-investigate {
  border: 3px solid #ff00ff;
  border-radius: 100px;
  cursor: pointer;
}

.clue {
  position: relative;
  width: 57px;
  height: 54px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.5em;

  img {
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
    z-index: -1;
  }
}

.game {
  width: 100%;
  z-index: 1;
}

.location-cards {
  display: flex;
  justify-content: center;
  align-items: center;
  overflow: auto;
  min-height: 350px;
}

.portrait {
  border-radius: 3px;
}

.portrait--can-move {
  cursor: pointer;
  border: 3px solid var(--select);
}

.location--can-move-to {
  border: 3px solid var(--select);
  cursor: pointer;
}

.agenda-container, .act-container {
  align-self: flex-start;
}

.discard {
  height: 100%;
  position: relative;
  &::after {
    pointer-events: none;
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: #FFF;
    /* background-image: linear-gradient(120deg, #eaee44, #33d0ff); */
    opacity: .85;
    mix-blend-mode: saturation;
  }
}

.question-label {
  text-align: center;
  background-color: var(--background);
  padding: 0;
  margin: 0;
  margin-top: 10px;
  overflow:auto;
}
</style>
