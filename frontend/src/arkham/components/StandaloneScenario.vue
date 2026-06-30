<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import StoryQuestion from '@/arkham/components/StoryQuestion.vue';
import Scenario from '@/arkham/components/Scenario.vue';
import ChooseDeck from '@/arkham/components/ChooseDeck.vue';
import ContinueCampaign from '@/arkham/components/ContinueCampaign.vue';
import { handleEmbeddedI18n } from '@/arkham/i18n';
import { useI18n } from 'vue-i18n';

const props = defineProps<{
  game: Game
  playerId: string
  realityAcidLightDevoured?: boolean
  realityAcidLightActive?: boolean
}>()

const emit = defineEmits<{
  update: [game: Game]
  choose: [idx: number]
  toggleRealityAcidLight: []
}>()

const { t } = useI18n()

async function update(game: Game) {
  emit('update', game);
}

async function choose(idx: number) {
  emit('choose', idx)
}

//const upgradeDeck = computed(() => props.game.campaign && props.game.campaign.step?.tag === 'UpgradeDeckStep')
const chooseDeck = computed(() => {
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

  return question?.tag === 'QuestionLabel' ? handleEmbeddedI18n(question.label, t) : null
})

const questionHash = computed(() => {
  let question = props.game.question[props.playerId]
  if (question) {
    return btoa(encodeURIComponent(JSON.stringify(question)))
  }

  return null
})

const continueCampaign = computed(() => {
  if (props.game.scenario?.campaignStep?.tag === 'ContinueCampaignStep') return props.game.scenario.campaignStep.contents
  return null
})

const inStep = computed(() => {
  return !!props.game.scenario?.campaignStep
})

// A PickScenarioSpecific question (e.g. Laid to Rest's spirit-deck builder) is a
// story-style question handled by StoryQuestion. It can be asked mid-Setup while
// the scenario is otherwise "active", so it must take priority over the board.
const storyQuestionOverride = computed(() =>
  Object.values(props.game.question).some((q) => q?.tag === 'PickScenarioSpecific')
)
</script>

<template>
  <div v-if="chooseDeck" id="game" class="game">
    <h2 v-if="questionLabel" class="question-label">{{ questionLabel }}</h2>
    <ChooseDeck :game="game" :playerId="playerId" @choose="choose" />
  </div>
  <div v-else-if="game.gameState.tag === 'IsActive'" id="game" class="game">
    <ContinueCampaign
      v-if="continueCampaign"
      :game="game"
      :scenario="game.scenario ?? undefined"
      :canUpgradeDecks="continueCampaign.canUpgradeDecks"
      :step="continueCampaign.nextStep"
      :chooseSideStory="continueCampaign.chooseSideStory"
      :canChooseSideStory="continueCampaign.canChooseSideStory"
    />
    <StoryQuestion
      v-else-if="storyQuestionOverride"
      :game="game"
      :key="questionHash ?? 'no-question'"
      :playerId="playerId"
      @choose="choose"
    />
    <Scenario
      v-else-if="game.scenario && game.phase !== 'CampaignPhase' && !inStep"
      :game="game"
      :scenario="game.scenario"
      :playerId="playerId"
      :realityAcidLightDevoured="realityAcidLightDevoured"
      :realityAcidLightActive="realityAcidLightActive"
      @choose="$emit('choose', $event)"
      @update="update"
      @toggleRealityAcidLight="$emit('toggleRealityAcidLight')"
    />
    <template v-else>
      <StoryQuestion :game="game" :key="questionHash ?? 'no-question'" :playerId="playerId" @choose="choose" />
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
  border: 3px solid var(--select);
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
    z-index: var(--z-index-neg-1);
  }
}

.game {
  width: 100%;
  z-index: var(--z-index-1);
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
</style>
