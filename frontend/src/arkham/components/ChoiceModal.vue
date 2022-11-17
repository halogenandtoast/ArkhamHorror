<script lang="ts" setup>
import { computed, inject, ref, watch, onMounted } from 'vue';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { QuestionType } from '@/arkham/types/Question';
import Card from '@/arkham/components/Card.vue';

export interface Props {
  game: Game
  investigatorId: string
}

const props = defineProps<Props>()

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
const focusedCards = computed(() => {
  const playerCards = Object.values(props.game.investigators[props.investigatorId].foundCards).flat()
  if (playerCards.length > 0) {
    return playerCards
  }

  const encounterCards = Object.values(props.game.foundCards).flat()
  if (encounterCards.length > 0) {
    return encounterCards
  }

  return props.game.focusedCards
})
const choosePaymentAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('choosePaymentAmounts')
const chooseAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('chooseAmounts')

const paymentAmountsLabel = computed(() => {
  const question = props.game.question[props.investigatorId]
  if (question?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.label
  }

  return null
})

const amountsLabel = computed(() => {
  const question = props.game.question[props.investigatorId]
  if (question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.label
  }

  if (question?.tag === QuestionType.QUESTION_LABEL && question?.question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.question.label
  }

  return null
})

const question = computed(() => props.game.question[props.investigatorId])

const investigatorName = (iid: string) => props.game.investigators[iid].name.title

const amountsChoices = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.value.paymentAmountChoices
  } else if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.amountChoices
  } else if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.question.amountChoices
  }

  return null
})

const amountSelections = ref<Record<string, number>>({})

const setInitialAmounts = () => {
    const labels = question.value?.tag === QuestionType.CHOOSE_AMOUNTS
      ? question.value.amountChoices.map((choice) => choice.label)
      : Object.keys(props.game.investigators)
    amountSelections.value = labels.reduce<Record<string, number>>((previousValue, currentValue) => {
      previousValue[currentValue] = 0
      return previousValue
    }, {})
  }

onMounted(setInitialAmounts)

watch(
  () => props.game.question[props.investigatorId],
  setInitialAmounts)

const unmetAmountRequirements = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    const requiredTotal = question.value.paymentAmountTargetValue
    if (requiredTotal) {
      const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
      return total !== requiredTotal
    }

    return false
  } else if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    switch(question.value.amountTargetValue.tag) {
      case 'MaxAmountTarget':
        {
          const maxBound = question.value.amountTargetValue.contents
          if (maxBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total > maxBound
          }
          break
        }
      case 'TotalAmountTarget':
        {
          const requiredTotal = question.value.amountTargetValue.contents
          if (requiredTotal) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total !== requiredTotal
          }
          break
        }
    }

    return false
  } else if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question.tag === QuestionType.CHOOSE_AMOUNTS) {
    const actual = question.value.question
    switch(actual.amountTargetValue.tag) {
      case 'MaxAmountTarget':
        {
          const maxBound = actual.amountTargetValue.contents
          if (maxBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total > maxBound
          }
          break
        }
      case 'TotalAmountTarget':
        {
          const requiredTotal = actual.amountTargetValue.contents
          if (requiredTotal) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total !== requiredTotal
          }
          break
        }
    }

    return false
  }

  return true
})

const submitPaymentAmounts = async () => {
  if (choosePaymentAmounts) {
    choosePaymentAmounts(amountSelections.value)
  }
}

const submitAmounts = async () => {
  if (chooseAmounts) {
    chooseAmounts(amountSelections.value)
  }
}
</script>

<template>
  <div v-if="focusedCards.length > 0 && choices.length > 0" class="modal">
    <div class="modal-contents focused-cards">
      <Card
        v-for="(card, index) in focusedCards"
        :card="card"
        :game="game"
        :investigatorId="investigatorId"
        :key="index"
        @choose="$emit('choose', $event)"
      />
    </div>
  </div>
  <div v-else-if="paymentAmountsLabel" class="modal amount-modal">
    <div class="modal-contents amount-contents">
      <form @submit.prevent="submitPaymentAmounts" :disabled="unmetAmountRequirements">
        <legend>{{paymentAmountsLabel}}</legend>
        <template v-for="amountChoice in amountsChoices" :key="amountChoice.investigatorId">
          <div v-if="amountChoice.maxBound !== 0">
            {{investigatorName(amountChoice.investigatorId)}}
            <input
              type="number"
              :min="amountChoice.minBound"
              :max="amountChoice.maxBound"
              v-model.number="amountSelections[amountChoice.investigatorId]"
              onclick="this.select()"
            />
          </div>
        </template>
        <button :disabled="unmetAmountRequirements">Submit</button>
      </form>
    </div>
  </div>
  <div v-else-if="amountsLabel" class="modal amount-modal">
    <div class="modal-contents amount-contents">
      <form @submit.prevent="submitAmounts" :disabled="unmetAmountRequirements">
        <legend>{{paymentAmountsLabel}}</legend>
        <template v-for="paymentChoice in amountsChoices" :key="paymentChoice.label">
          <div v-if="paymentChoice.maxBound !== 0">
            {{paymentChoice.label}} <input type="number" :min="paymentChoice.minBound" :max="paymentChoice.maxBound" v-model.number="amountSelections[paymentChoice.label]" onclick="this.select()" />
          </div>
        </template>
        <button :disabled="unmetAmountRequirements">Submit</button>
      </form>
    </div>
  </div>
</template>

<style scoped lang="scss">
.modal-contents {
  background: white;
  display: flex;
  align-items: center;
  padding: 10px 0;
}

.focused-cards {
  flex-direction: row;
  overflow-x: auto;
  flex-wrap: wrap;
}

.amount-contents {
  background: #735e7b;
  padding: 10px;
  div {
    display: inline;
  }
  button {
    background: #4a3d50;
    display: inline;
    border: 0;
    color: white;
    padding: 0.5em;
    margin-left: 0.5em;
  }

  button[disabled] {
    cursor: not-allowed;
  }

  input {
    padding: 0.5em;
  }

  legend {
    font-size: 1.2em;
    font-weight: bold;
  }

  .selection {
    margin-left: 50px;
    &:nth-of-type(1) {
      margin-left: 0;
    }
  }
}
</style>
