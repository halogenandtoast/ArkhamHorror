<script lang="ts" setup>
import { computed, inject, ref, watch, onMounted } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import * as ArkhamGame from '@/arkham/types/Game';
import { AmountChoice, QuestionType } from '@/arkham/types/Question';
import Card from '@/arkham/components/Card.vue';
import Draggable from '@/components/Draggable.vue';

const props = defineProps<{
  game: Game
  playerId: string
}>()

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const investigator = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId))
const focusedCards = computed(() => {
  const playerCards = Object.values(investigator.value?.foundCards ?? []).flat()
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
  const question = props.game.question[props.playerId]
  if (question?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.label
  }

  return null
})

const amountsLabel = computed(() => {
  const question = props.game.question[props.playerId]
  if (question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.label
  }

  if (question?.tag === QuestionType.QUESTION_LABEL && question?.question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.question.label
  }

  return null
})

const question = computed(() => props.game.question[props.playerId])

const investigatorName = (iid: string) => props.game.investigators[iid].name.title

const paymentAmountsChoices = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.value.paymentAmountChoices
  }

  return []
})

const chooseAmountsChoices = computed<AmountChoice[]>(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.amountChoices
  } else if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.question.amountChoices
  }

  return []
})

const amountSelections = ref<Record<string, number>>({})

const setInitialAmounts = () => {
    const labels = question.value?.tag === QuestionType.CHOOSE_AMOUNTS
      ? question.value.amountChoices.map((choice) => choice.label)
      : (paymentAmountsChoices.value ?? []).map((choice) => choice.title)
    amountSelections.value = labels.reduce<Record<string, number>>((previousValue, currentValue) => {
      previousValue[currentValue] = 0
      return previousValue
    }, {})
  }

onMounted(setInitialAmounts)

watch(
  () => props.game.question[props.playerId],
  setInitialAmounts)

const unmetAmountRequirements = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    const target = question.value.paymentAmountTargetValue
    if (target) {
      switch(target.tag) {
        case 'MaxAmountTarget':
          {
            const maxBound = target.contents
            if (maxBound) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return total > maxBound
            }
            break
          }
        case 'MinAmountTarget':
          {
            const minBound = target.contents
            if (minBound) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return total < minBound
            }
            break
          }
        case 'TotalAmountTarget':
          {
            const requiredTotal = target.contents
            if (requiredTotal) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return total !== requiredTotal
            }
            break
          }
        case 'AmountOneOf':
          {
            const totals = target.contents
            if (totals.length > 0) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return totals.indexOf(total) === -1
            }
            break
          }
      }
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
      case 'MinAmountTarget':
        {
          const minBound = question.value.amountTargetValue.contents
          if (minBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total < minBound
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
      case 'AmountOneOf':
        {
          const totals = question.value.amountTargetValue.contents
          if (totals.length > 0) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return totals.indexOf(total) === -1
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
      case 'MinAmountTarget':
        {
          const minBound = actual.amountTargetValue.contents
          if (minBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total < minBound
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
      case 'AmountOneOf':
        {
          const totals = question.value.amountTargetValue.contents
          if (totals.length > 0) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return totals.indexOf(total) === -1
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

const skillTestResults = computed(() => props.game.skillTestResults)

const cardLabels = computed(() =>
  choices.value.
    flatMap<[Message, number][]>((choice, index) => {
      return choice.tag === "CardLabel" ? [[choice, index]] : []
    }))

const showChoices = computed(() =>
  focusedCards.value.length > 0 || paymentAmountsLabel.value || amountsLabel.value
)

const title = computed(() => {
  if (question.value && question.value.tag === 'QuestionLabel') {
    return replaceIcons(question.value.label)
  }

  if (skillTestResults.value) {
    return "Results"
  }

  if (cardLabels.value.length > 0) {
    return "Choose"
  }

  if (question.value && question.value.tag === QuestionType.READ) {
    if (question.value.flavorText.title) {
      return question.value.flavorText.title
    }

    return "Story"
  }

  if (showChoices.value) {
    return "Choose"
  }

  return null
})

const replaceIcons = function(body: string) {
  return body.
    replace('{action}', '<span class="action-icon"></span>').
    replace('{fast}', '<span class="fast-icon"></span>').
    replace('{willpower}', '<span class="willpower-icon"></span>').
    replace('{intellect}', '<span class="intellect-icon"></span>').
    replace('{combat}', '<span class="combat-icon"></span>').
    replace('{agility}', '<span class="agility-icon"></span>').
    replace('{wild}', '<span class="wild-icon"></span>').
    replace('{elderSign}', '<span class="elder-sign"></span>')
}
</script>

<template>
  <Draggable v-if="showChoices">
  <template #handle><h1 v-html="title"></h1></template>
    <div v-if="focusedCards.length > 0 && choices.length > 0" class="modal">
      <div class="modal-contents focused-cards">
        <Card
          v-for="(card, index) in focusedCards"
          :card="card"
          :game="game"
          :playerId="playerId"
          :key="index"
          @choose="$emit('choose', $event)"
        />
      </div>

      <div v-if="showChoices" class="choices">
        <template v-for="(choice, index) in choices" :key="index">
          <div v-if="choice.tag === MessageType.LABEL">
            <button @click="$emit('choose', index)" v-html="replaceIcons(choice.label)"></button>
          </div>
        </template>
      </div>
    </div>
    <div v-else-if="paymentAmountsLabel" class="modal amount-modal">
      <div class="modal-contents amount-contents">
        <form @submit.prevent="submitPaymentAmounts" :disabled="unmetAmountRequirements">
          <legend>{{paymentAmountsLabel}}</legend>
          <template v-for="amountChoice in paymentAmountsChoices" :key="amountChoice.investigatorId">
            <div v-if="amountChoice.maxBound !== 0">
              {{amountChoice.title}}
              <input
                type="number"
                :min="amountChoice.minBound"
                :max="amountChoice.maxBound"
                v-model.number="amountSelections[amountChoice.title]"
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
          <template v-for="paymentChoice in chooseAmountsChoices" :key="paymentChoice.label">
            <div v-if="paymentChoice.maxBound !== 0">
              {{paymentChoice.label}} <input type="number" :min="paymentChoice.minBound" :max="paymentChoice.maxBound" v-model.number="amountSelections[paymentChoice.label]" onclick="this.select()" />
            </div>
          </template>
          <button :disabled="unmetAmountRequirements">Submit</button>
        </form>
      </div>
    </div>
  </Draggable>
</template>

<style scoped lang="scss">
.modal-contents {
  display: flex;
  align-items: center;
  padding: 10px;
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

.choices {
  padding-bottom: 20px;
  text-align: center;
  button {
    transition: all 0.3s ease-in;
    border: 0;
    padding: 10px;
    text-transform: uppercase;
    background-color: #532e61;
    font-weight: bold;
    border-radius: 0.6em;
    color: #EEE;
    font: Arial, sans-serif;
    &:hover {
      background-color: #311b3e;
    }
  }
}
</style>
