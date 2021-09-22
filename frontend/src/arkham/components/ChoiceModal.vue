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
    <div class="modal-contents focused-cards">
      <form @submit.prevent="submitAmounts">
        <h1>{{amountsLabel}}</h1>
        <div v-for="[investigator, bounds] in amountsChoices" :key="investigator">
          {{investigatorName(investigator)}} <input type="number" :min="bounds[0]" :max="bounds[1]" v-model.number="amountSelections[investigator]" />
        </div>
        <button>Submit</button>
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
      if (question.tag == 'ChoosePaymentAmounts') {
        return question.contents[0]
      } else if (question.tag == 'ChooseDynamicCardAmounts') {
        return "Pay Dynamic Cost"
      }

      return null
    })

    const investigatorName = (iid: string) => props.game.investigators[iid].contents.name.title

    const amountsChoices = computed(() => {
      const question = props.game.question[props.investigatorId]
      if (question.tag == 'ChoosePaymentAmounts') {
        return question.contents[2]
      } else if (question.tag == 'ChooseDynamicCardAmounts') {
        return [[question.contents[0], question.contents[2]]]
      }

      return null
    })

    const amountSelections = ref(Object.keys(props.game.investigators).reduce<Record<string, number>>((previousValue, currentValue) => {
      previousValue[currentValue] = 0
      return previousValue
    }, {}))

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

    return { choices, focusedCards, resolutions, amountsLabel, amountsChoices, amountSelections, investigatorName, submitAmounts }
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
</style>
