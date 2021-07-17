<template>
  <div>
    <img
      v-if="investigatorPortrait"
      class="portrait"
      :src="investigatorPortrait"
    />
    <img
      class="deck"
      :src="`${baseUrl}/img/arkham/back.png`"
      :class="{ 'can-interact': deckAction !== -1 }"
      @click="$emit('choose', deckAction)"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { MessageType } from '@/arkham/types/Message';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://arkham-horror-assets.s3.amazonaws.com" : '';
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
    const drawEncounterCardAction = computed(() => {
      return choices.value.findIndex((c) => c.tag === MessageType.INVESTIGATOR_DRAW_ENCOUNTER_CARD)
    })

    const searchTopOfEncounterCardAction = computed(() => {
      return choices.value.findIndex((c) => c.tag === MessageType.SEARCH_TOP_OF_DECK && c.contents[2].tag === 'EncounterDeckTarget');
    })

    const surgeAction = computed(() => choices.value.findIndex((c) => c.tag === MessageType.SURGE))

    const deckAction = computed(() => {
      if (drawEncounterCardAction.value !== -1) {
        return drawEncounterCardAction.value
      }

      if (surgeAction.value !== -1) {
        return surgeAction.value
      }

      return searchTopOfEncounterCardAction.value
    })

    const investigatorPortrait = computed(() => {
      const choice = choices.value[deckAction.value]

      if (!choice) {
        return null;
      }

      switch (choice.tag) {
        case MessageType.INVESTIGATOR_DRAW_ENCOUNTER_CARD:
          return `${baseUrl}/img/arkham/portraits/${choice.contents}.jpg`;
        case MessageType.SURGE:
          return `${baseUrl}/img/arkham/portraits/${choice.contents}.jpg`;
        default:
          return `${baseUrl}/img/arkham/portraits/${choice.contents[0]}.jpg`;
      }
    })

    return { baseUrl, investigatorPortrait, deckAction, surgeAction, searchTopOfEncounterCardAction, drawEncounterCardAction }
  }
})
</script>

<style scoped lang="scss">
.deck {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: 100px;
}
.portrait {
  width: 100px;
}
.can-interact {
  border: 3px solid #FF00FF;
  cursor: pointer;
}
</style>
