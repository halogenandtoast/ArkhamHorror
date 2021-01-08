<template>
  <div class="card-container">
    <img
      :class="{'card--can-interact': cardAction !== -1}"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Card } from '@/arkham/types/Card';
import { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    card: { type: Object as () => Card, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const image = computed(() => {
      const { cardCode } = props.card.contents;
      return `/img/arkham/cards/${cardCode}.jpg`;
    })

    const id = computed(() => props.card.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canInteract(c: Message): boolean {
      switch (c.tag) {
        case MessageType.DISCARD:
          // TODO: Check the contents tag
          return c.contents.contents[1] === id.value;
        case MessageType.ADD_FOCUSED_TO_HAND:
          return c.contents[2] === id.value;
        case MessageType.ADD_FOCUSED_TO_TOP_OF_DECK:
          return c.contents[2] === id.value;
        case MessageType.FOUND_AND_DREW_ENCOUNTER_CARD:
          return c.contents[2].id === id.value;
        case MessageType.FOUND_ENCOUNTER_CARD_FROM:
          return c.contents[3].id === id.value;
        case MessageType.FOUND_ENEMY_IN_VOID:
          return c.contents[2] === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canInteract(c1));
        default:
          return false;
      }
    }

    const cardAction = computed(() =>  choices.value.findIndex(canInteract))

    return { cardAction, image }
  }
})
</script>

<style scoped lang="scss">

.card {
  width: 100px;
  min-width: 100px;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  display: inline-block;

  &--can-interact {
    border: 2px solid #FF00FF;
    cursor: pointer;
  }
}
</style>
