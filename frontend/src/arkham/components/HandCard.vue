<template>
  <div class="card-container">
    <img
      :class="classObject"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />

  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import { Card } from '@/arkham/types/Card'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { Message, MessageType } from '@/arkham/types/Message'

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    card: { type: Object as () => Card, required: true },
    investigatorId: { type: String, required: true }
  },
  setup(props) {
    const id = computed(() => props.card.contents.id)
    const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

    function canUncommit(c: Message): boolean {
      switch (c.tag) {
        case MessageType.UNCOMMIT_CARD:
          return c.contents[1] === id.value
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canUncommit(c1))
        default:
          return false
      }
    }
    const uncommitCardAction = computed(() => choices.value.findIndex(canUncommit))

    function canPlay(c: Message): boolean {
      switch (c.tag) {
        case MessageType.PLAY_CARD:
          return c.contents[1] === id.value
        case MessageType.LEGACY_PLAY_CARD:
          return c.contents[1] === id.value
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canPlay(c1))
        default:
          return false
      }
    }

    function canReveal(c: Message): boolean {
      switch (c.tag) {
        case MessageType.REVEAL_CARD:
          return c.contents === id.value
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canReveal(c1))
        default:
          return false
      }
    }

    function canDiscard(c: Message): boolean {
      switch (c.tag) {
        case MessageType.DISCARD_CARD:
          return c.contents[1] === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canDiscard(c1));
        default:
          return false;
      }
    }

    function canCommit(c: Message): boolean {
      switch (c.tag) {
        case MessageType.COMMIT_CARD:
          return c.contents[1] === id.value;
        case MessageType.RUN:
          return c.contents.some((c1: Message) => canCommit(c1));
        default:
          return false;
      }
    }

    const discardCardAction = computed(() => choices.value.findIndex(canDiscard))
    const playCardAction = computed(() => choices.value.findIndex(canPlay))
    const revealCardAction = computed(() => choices.value.findIndex(canReveal))
    const commitCardAction = computed(() => choices.value.findIndex(canCommit))

    const cardAction = computed(() => {
      if (revealCardAction.value !== -1) {
        return revealCardAction.value
      }

      if (playCardAction.value !== -1) {
        return playCardAction.value
      }

      if (uncommitCardAction.value !== -1) {
        return uncommitCardAction.value
      }

      if (discardCardAction.value !== -1) {
        return discardCardAction.value
      }

      return commitCardAction.value
    })

    const classObject = computed(() => {
      return {
        'card--can-interact': cardAction.value !== -1,
        'card--committed': uncommitCardAction.value !== -1,
      }
    })

    const image = computed(() => {
      const { cardCode } = props.card.contents.def;
      return `/img/arkham/cards/${cardCode}.jpg`;
    })

    return { image, classObject, cardAction }
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

  &--committed {
    margin-top: -10px;
  }
}
</style>
