<template>
  <div v-if="focusedCards.length > 0 && choices.length > 0" class="modal">
    <div class="modal-contents focused-cards">
      <FocusedCard
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
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';
import FocusedCard from '@/arkham/components/FocusedCard.vue';

export default defineComponent({
  components: {
    FocusedCard,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
    const focusedCards = computed(() => props.game.focusedCards)

    const resolutions = computed(() => {
      return choices
        .value
        .map((choice, idx) => ({ choice, idx }))
        .filter(({ choice }) => choice.tag === MessageType.RESOLUTION);
    })

    return { choices, focusedCards, resolutions }
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
}
</style>
