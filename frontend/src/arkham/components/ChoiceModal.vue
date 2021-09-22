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
  <div v-else-if="amountsLabel" class="modal amount-modal">
    <div class="modal-contents amount-contents">
      <form @submit.prevent="submitAmounts" :disabled="unmetAmountRequirements">
        <legend>{{amountsLabel}}</legend>
        <div v-for="[investigator, bounds] in amountsChoices" :key="investigator" class="selection">
          {{investigatorName(investigator)}} <input type="number" :min="bounds[0]" :max="bounds[1]" v-model.number="amountSelections[investigator]" onclick="this.select()" />
        </div>
        <button :disabled="unmetAmountRequirements">Submit</button>
      </form>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed, inject, ref } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import Card from '@/arkham/components/Card.vue';

export default defineComponent({
  components: {
    Card,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
    const focusedCards = computed(() => props.game.focusedCards)
    const chooseAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('chooseAmounts')

    const amountsLabel = computed(() => {
      const question = props.game.question[props.investigatorId]
      if (question?.tag == 'ChoosePaymentAmounts') {
        return question.contents[0]
      } else if (question?.tag == 'ChooseDynamicCardAmounts') {
        return "Pay Dynamic Cost"
      }

      return null
    })

    const investigatorName = (iid: string) => props.game.investigators[iid].contents.name.title

    const amountsChoices = computed(() => {
      const question = props.game.question[props.investigatorId]
      if (question?.tag == 'ChoosePaymentAmounts') {
        return question.contents[2]
      } else if (question?.tag == 'ChooseDynamicCardAmounts') {
        return [[question.contents[0], question.contents[2]]]
      }

      return null
    })

    const amountSelections = ref(Object.keys(props.game.investigators).reduce<Record<string, number>>((previousValue, currentValue) => {
      previousValue[currentValue] = 0
      return previousValue
    }, {}))

    const unmetAmountRequirements = computed(() => {
      const question = props.game.question[props.investigatorId]
      if (question?.tag == 'ChoosePaymentAmounts') {
        const requiredTotal = question.contents[1]
        if (requiredTotal) {
          const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
          return total !== requiredTotal
        }

        return false
      } else if (question?.tag == 'ChooseDynamicCardAmounts') {
        return false
      }

      return true
    })

    const submitAmounts = async () => {
      if (chooseAmounts) {
        chooseAmounts(amountSelections.value)
      }
    }


    const resolutions = computed(() => {
      return choices
        .value
        .map((choice, idx) => ({ choice, idx }))
        .filter(({ choice }) => choice.tag === MessageType.RESOLUTION);
    })

    return { choices, focusedCards, resolutions, amountsLabel, amountsChoices, amountSelections, investigatorName, submitAmounts, unmetAmountRequirements }
  }
})
</script>

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
