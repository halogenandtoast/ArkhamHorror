<script lang="ts" setup>
import { computed, ref } from 'vue';
import { useI18n } from 'vue-i18n';
import type { Game } from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import * as ArkhamGame from '@/arkham/types/Game';
import { choiceRequiresModal } from '@/arkham/types/Message';
import { replaceIcons } from '@/arkham/helpers';
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
const hide = ref(false)
const { t } = useI18n()

async function choose(idx: number) {
  emit('choose', idx)
}

const inSkillTest = computed(() => props.game.skillTest !== null)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const investigator = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId))

const searchedCards = computed(() => {
  const playerCards = Object.entries(investigator.value?.foundCards ?? [])

  const playerZones = playerCards.filter(([, c]) => c.length > 0)

  const encounterCards = Object.entries(props.game.foundCards)
  const encounterZones = encounterCards.filter(([, c]) => c.length > 0)

  return [...playerZones, ...encounterZones]
})

const focusedCards = computed(() => {
  if (searchedCards.value.length > 0) {
    return []
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

const requiresModal = computed(() => {
  if (props.noStory && question.value?.tag === QuestionType.READ) {
    return false
  }
  if (inSkillTest.value) {
    return false
  }
  return (props.game.focusedChaosTokens.length > 0 && !inSkillTest.value) || focusedCards.value.length > 0 || searchedCards.value.length > 0 || paymentAmountsLabel.value || amountsLabel.value || choicesRequireModal.value || question.value.tag === 'QuestionLabel'
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
  if (body.startsWith("$")) {
    return t(body.slice(1))
  }
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
}

const skillTestResults = computed(() => props.game.skillTestResults)

const title = computed(() => {
  if (question.value && question.value.tag === 'QuestionLabel') {
    return replaceIcons(question.value.label)
  }

  if (skillTestResults.value) {
    return "Results"
  }

  if (question.value && question.value.tag === QuestionType.READ) {
    if (question.value.flavorText.title) {
      return question.value.flavorText.title
    }

    return "Story"
  }

  if (question.value && question.value.tag === QuestionType.DROP_DOWN) {
    return "Choose one"
  }


  if (amountsLabel.value) {
    return amountsLabel.value
  }

  return "Choose"
})
</script>

<template>
  <Draggable v-if="requiresModal">
  <template #handle><h1 v-html="label(title)"></h1></template>
    <Question v-if="question" :game="game" :playerId="playerId" @choose="choose" />
  </Draggable>
</template>

<style scoped lang="scss">
</style>
