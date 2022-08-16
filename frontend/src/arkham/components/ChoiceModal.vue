<script lang="ts" setup>
import { computed, inject, ref, watch, onMounted } from 'vue';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
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
  if (question?.tag == 'ChoosePaymentAmounts') {
    return question.contents[0]
  } else if (question?.tag == 'ChooseDynamicCardAmounts') {
    return "Pay Dynamic Cost"
  }

  return null
})

const amountsLabel = computed(() => {
  const question = props.game.question[props.investigatorId]
  if (question?.tag == 'ChooseAmounts') {
    return question.contents[0]
  }

  return null
})

const question = computed(() => props.game.question[props.investigatorId])

const investigatorName = (iid: string) => props.game.investigators[iid].name.title

const amountsChoices = computed(() => {
  if (question.value?.tag == 'ChoosePaymentAmounts') {
    return question.value.contents[2]
  } else if (question.value?.tag == 'ChooseAmounts') {
    return question.value.contents[2]
  } else if (question.value?.tag == 'ChooseDynamicCardAmounts') {
    return [[question.value.contents[0], question.value.contents[2]]]
  }

  return null
})

const amountSelections = ref<Record<string, number>>({})

const setInitialAmounts = () => {
    const labels = question.value?.tag == 'ChooseAmounts'
      ? question.value.contents[2].map(([label]) => label)
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
  if (question.value?.tag == 'ChoosePaymentAmounts') {
    const requiredTotal = question.value.contents[1]
    if (requiredTotal) {
      const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
      return total !== requiredTotal
    }

    return false
  } else if (question.value?.tag == 'ChooseAmounts') {
    const maxBound = question.value.contents[1]
    if (maxBound) {
      const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
      return total > maxBound
    }

    return false
  } else if (question.value?.tag == 'ChooseDynamicCardAmounts') {
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

const resolutions = computed(() => {
  return choices
    .value
    .map((choice, idx) => ({ choice, idx }))
    .filter(({ choice }) => choice.tag === "WOMBAT");
})
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
  <div v-else-if="resolutions.length > 0" class="modal">
    <div class="modal-contents">
      <button
        v-for="{ choice, idx } in resolutions"
        :key="idx"
        @click="$emit('choose', idx)"
      >
        Resolution {{choice.contents}}
      </button>
    </div>
  </div>
  <div v-else-if="paymentAmountsLabel" class="modal amount-modal">
    <div class="modal-contents amount-contents">
      <form @submit.prevent="submitPaymentAmounts" :disabled="unmetAmountRequirements">
        <legend>{{paymentAmountsLabel}}</legend>
        <template v-for="[investigator, bounds] in amountsChoices" :key="investigator">
          <div v-if="bounds[1] !== 0">
            {{investigatorName(investigator)}} <input type="number" :min="bounds[0]" :max="bounds[1]" v-model.number="amountSelections[investigator]" onclick="this.select()" />
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
        <template v-for="[label, bounds] in amountsChoices" :key="label">
          <div v-if="bounds[1] !== 0">
            {{label}} <input type="number" :min="bounds[0]" :max="bounds[1]" v-model.number="amountSelections[label]" onclick="this.select()" />
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
