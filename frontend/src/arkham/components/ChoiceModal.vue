<script lang="ts" setup>
import { computed, inject, ref, watch, onMounted } from 'vue';
import { useI18n } from 'vue-i18n';
import type { Game } from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import * as ArkhamGame from '@/arkham/types/Game';
import { replaceIcons } from '@/arkham/helpers';
import { AmountChoice, QuestionType } from '@/arkham/types/Question';
import Card from '@/arkham/components/Card.vue';
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

function zoneToLabel(s: string) {
  switch(s) {
    case "FromDeck": return "From Deck"
    case "FromHand": return "From Hand"
    case "FromDiscard": return "From Discard"
    default: return s
  }
}

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
const choosePaymentAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('choosePaymentAmounts')
const chooseAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('chooseAmounts')
const question = computed(() => props.game.question[props.playerId])

const paymentAmountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return replaceIcons(question.value.label)
  }

  return null
})

const amountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return replaceIcons(question.value.label)
  }

  if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.question.label
  }

  return null
})

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

const doneLabel = computed(() => {
  const doneIndex = choices.value.findIndex((c) => c.tag === MessageType.DONE)

  if (doneIndex !== -1) {
    return { label: choices.value[doneIndex].label, index: doneIndex } // choices.value[doneIndex].label
  }

  return null
})

const label = function(body: string) {
  if (body.startsWith("$")) {
    return t(body.slice(1))
  }
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
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
          const totals = actual.amountTargetValue.contents
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
  focusedChaosTokens.value.length > 0 || focusedCards.value.length > 0 || searchedCards.value.length > 0 || paymentAmountsLabel.value || amountsLabel.value || modalChoices.value.length > 0 || cardLabels.value.length > 0 || (question.value && question.value.tag === "DropDown")
)

function isModalChoice(choice: Message) {
  return choice.tag === MessageType.LABEL || choice.tag === MessageType.SKILL_LABEL || choice.tag === MessageType.SKILL_LABEL_WITH_LABEL || choice.tag == MessageType.PORTRAIT_LABEL
}

const modalChoices = computed(() => {
  if (question.value) {
    return choices.value.filter(isModalChoice)
  }
  return []
})

const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

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

    if (props.noStory) {
      return null
    }

    return "Story"
  }

  if (question.value && question.value.tag === QuestionType.DROP_DOWN) {
    return "Choose one"
  }


  if (amountsLabel.value) {
    return amountsLabel.value
  }

  if (showChoices.value) {
    return "Choose"
  }

  if (focusedChaosTokens.value.length > 0) {
    return "Choose"
  }


  return null
})
</script>

<template>
  <Draggable v-if="title && showChoices">
  <template #handle><h1 v-html="label(title)"></h1></template>
    <Question v-if="question" :game="game" :playerId="playerId" @choose="choose" />
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
    </div>
    <div v-if="searchedCards.length > 0 && choices.length > 0" class="modal">
      <div class="modal-contents searched-cards">
        <div v-for="[group, cards] in searchedCards" :key="group" class="group">
          <h2>{{zoneToLabel(group)}}</h2>
          <div class="group-cards">
            <Card
              v-for="card in cards"
              :card="card"
              :game="game"
              :playerId="playerId"
              :key="`${group}-${card}`"
              @choose="$emit('choose', $event)"
            />
          </div>
        </div>
      </div>
    </div>
    <div v-if="paymentAmountsLabel" class="modal amount-modal">
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
    <div v-if="amountsLabel" class="modal amount-modal">
      <div v-if="searchedCards.length > 0" class="modal-contents searched-cards">
        <div v-for="[group, cards] in searchedCards" :key="group" class="group">
          <h2>{{zoneToLabel(group)}}</h2>
          <div class="group-cards">
            <Card
              v-for="card in cards"
              :card="card"
              :game="game"
              :playerId="playerId"
              :key="`${group}-${card}`"
              @choose="$emit('choose', $event)"
            />
          </div>
        </div>
      </div>
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

    <div v-if="doneLabel">
      <button class="done" @click="$emit('choose', doneLabel.index)">{{doneLabel.label}}</button>
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
  box-sizing: border-box;
  border-bottom-left-radius: 15px;
  border-bottom-right-radius: 15px;
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

.searched-cards {
  flex-direction: column;
  overflow-x: auto;
}

.group {
  display: flex;
  align-items: center;
  flex-direction: column;
  .group-cards {
    display: flex;
    flex-wrap: wrap;
  }
}

.done {
  width: 100%;
  border: 0;
  margin: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0 0 1.2em 1.2em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
  }
}

i {
  font-family: 'Arkham';
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
}

i.iconSkull {
  &:before {
    font-family: "Arkham";
    content: "\004E";
  }
}

i.iconCultist {
  &:before {
    font-family: "Arkham";
    content: "\0042";
  }
}

i.iconTablet {
  &:before {
    font-family: "Arkham";
    content: "\0056";
  }
}

i.iconElderThing {
  &:before {
    font-family: "Arkham";
    content: "\0043";
  }
}

i.iconSkillWillpower {
  &:before {
    font-family: "Arkham";
    content: "\0041";
  }
}

i.iconSkillIntellect {
  &:before {
    font-family: "Arkham";
    content: "\0046";
  }
}

i.iconSkillCombat {
  &:before {
    font-family: "Arkham";
    content: "\0044";
  }
}

i.iconSkillAgility {
  &:before {
    font-family: "Arkham";
    content: "\0053";
  }
}

section {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: center;
}

.button {
  display: inline-block;
  padding: 5px 10px;
  margin: 2px;
  background-color: #333;
  color: white;
  border: 1px solid #666;
  cursor: pointer;

  &:hover {
    background-color: #111;
  }

  &:active {
    background-color: #666;
    border-color: #111;
  }
}

.intro-text {
  width: 50vw;
  text-align: justify;
  background: linear-gradient(#DFDAD8, darken(#DFDAD8, 10%));
  padding: 10px;
  margin: 10px;
  margin-bottom: 0;
  border-radius: 5px;
  box-sizing: border-box;
  font-size: 1.1em;
  -moz-osx-font-smoothing: grayscale;
  -webkit-font-smoothing: antialiased !important;
  -moz-font-smoothing: antialiased !important;
  text-rendering: optimizelegibility !important;
  letter-spacing: .03em;
  h1 {
    font-family: "Teutonic";
    font-weight: 500;
    color: #38615F;
    margin: 0;
    padding-bottom: 2px;
    margin-bottom: 10px;
    border-bottom: 1px solid #38615f;
    &::after {
      display: block;
      content: " ";
      margin-top: 2px;
      border-bottom: 1px solid #38615f;
    }
  }

  p {
    margin: 10px;
  }
}

.status-bar {
  text-align: center;
}

button {
  transition: all 0.3s ease-in;
  border: 0;
  padding: 10px;
  background-color: #532e61;
  text-align: justify;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
  }
}

.card {
  width: $card-width;
  margin: 2px;
}

.question-label {
  display: flex;
  flex-direction: column;
  width: 75%;

  p {
    font-size: 2em;
  }
}

.label-choices {
  display: flex;
  flex-direction: row;
  align-self: center;
  button {
    margin-left: 10px;
  }
}

.portrait {
  border-radius: 3px;
  width: $card-width;
  margin-right: 2px;

  &.active {
    border: 1px solid $select;
    cursor:pointer;
  }
}

.status-bar:empty {
  display: none;
}

.hide {
  opacity: 0;
}

.modal {
  transition: opacity 0.3s linear;
}

.choices {
  display: flex;
  flex-direction: column;
  gap: 10px;
  width: 100%;
  margin-inline: 10px;
  button {
    font-size: 1.2em;
    width: 100%;
    white-space: nowrap;
    text-wrap: pretty;
    &:before {
      font-family: "ArkhamIcons";
      content: "\E91A";
      margin-right: 10px;
    }
  }

  &:has(.portrait) {
    flex-direction: row;
    padding: 10px;
    justify-content: center;
  }
}

p {
  font-family: "ArkhamFlavor";
  :deep(i) {
    font-family: "ArkhamCursive";
    text-align: center;
    display: block;
    font-size: 1.3em;
    line-height: 0.3em;
    &:last-child {
      padding-bottom: 1.3em;
    }
  }
}

.message-label {
  margin-top: 10px;
  margin-bottom: 10px;
}

h2 {
  font-family: "Teutonic";
  letter-spacing: 1px;
  font-size: 1.7em;
}
</style>
