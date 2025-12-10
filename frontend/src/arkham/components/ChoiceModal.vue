<script lang="ts" setup>
import { computed } from 'vue';
import { useI18n } from 'vue-i18n';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { choiceRequiresModal } from '@/arkham/types/Message';
import { formatContent, replaceIcons } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { QuestionType } from '@/arkham/types/Question';
import Draggable from '@/components/Draggable.vue';
import Question from '@/arkham/components/Question.vue';

export interface Props {
  game: Game
  playerId: string
  noStory?: boolean
}

const props = withDefaults(defineProps<Props>(), { noStory: false })
const emit = defineEmits(['choose'])
const { t, te } = useI18n()

async function choose(idx: number) {
  emit('choose', idx)
}

const inSkillTest = computed(() => props.game.skillTest !== null)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const investigator = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId))
const searchedCards = computed(() => {
  const playerCards = Object.entries(investigator.value?.foundCards ?? [])

  const playerZones = playerCards.filter(([, c]) => c.length > 0)

  const encounterCards = Object.entries(props.game.scenario?.foundCards ? props.game.scenario.foundCards : props.game.foundCards)
  const encounterZones = encounterCards.filter(([, c]) => c.length > 0)

  return [...playerZones, ...encounterZones]
})

const focusedCards = computed(() => {
  if (searchedCards.value.length > 0) {
    return []
  }

  const { focusedCards, foundCards } = props.game

  if (focusedCards.length === 0) {
    if (Object.values(props.game.foundCards).some((v) => v.length > 0)) {
      return Object.values(props.game.foundCards).flat()
    }
  }

  return props.game.focusedCards
})

const paymentAmountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return replaceIcons(question.value.label)
  }

  return null
})

const choicesRequireModal = computed(() => choices.value.some(choiceRequiresModal))

const tokenChoices = computed(() => props.game.scenario?.chaosBag.choice)

const requiresModal = computed(() => {
  if (props.noStory && question.value?.tag === QuestionType.READ) {
    return false
  }
  if (inSkillTest.value) {
    return false
  }

  return ((props.game.focusedChaosTokens.length > 0 || tokenChoices.value !== null) && !inSkillTest.value) || focusedCards.value.length > 0 || searchedCards.value.length > 0 || paymentAmountsLabel.value || amountsLabel.value || choicesRequireModal.value || ['QuestionLabel', 'DropDown', 'ChooseExchangeAmounts'].includes(question.value?.tag)
})

const question = computed(() => props.game.question[props.playerId])

const amountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return replaceIcons(question.value.label)
  }

  if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.question.label
  }

  return null
})

const label = function(body: string) {
  return formatContent(body.startsWith("$") ? handleI18n(body.slice(1), t) : body)
}

const skillTestResults = computed(() => props.game.skillTestResults)

const body = computed(() => {
  if (question.value && question.value.tag === 'QuestionLabel') {
    if (question.value.label !== "@none") {
      return question.value.label
    }
  }

  return null
})

const title = computed(() => {
  if (skillTestResults.value) {
    return t("Results")
  }

  if (question.value && question.value.tag === QuestionType.READ) {
    if (question.value.flavorText.title) {
      return question.value.flavorText.title
    }

    return t("Story")
  }

  if (question.value && question.value.tag === QuestionType.DROP_DOWN) {
    return t("Choose one")
  }


  if (amountsLabel.value) {
    if(amountsLabel.value.startsWith("$")) {
      let titleKey = amountsLabel.value.replace(".label.", ".title.")
      return te(titleKey.slice(1)) ? titleKey : amountsLabel.value
    } else {
      return amountsLabel.value
    }
  }

  if (!question.value) {
    return ""
  }

  return t("Choose")
})
</script>

<template>
  <Draggable v-if="requiresModal">
    <template #handle><h1 v-html="label(title)"></h1></template>
    <div class='choice-modal-wrapper'>
      <p class="body" v-if="body" v-html="label(body)"></p>
      <Question v-if="question" :game="game" :playerId="playerId" @choose="choose" />
    </div>
  </Draggable>
</template>

<style scoped>
.body {
  font-size: 1.3em;
  font-family: "Noto Sans", sans-serif;
  color: var(--title);
  background: rgba(0, 0, 0, 0.6);
  padding: 10px;
  border-radius: 10px;
  border: 1px solid #111;
}

.choice-modal-wrapper {
  display: flex;
  flex-direction: column;
  gap: 10px;
}
</style>
