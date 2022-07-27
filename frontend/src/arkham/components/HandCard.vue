<script lang="ts" setup>
import { computed, inject } from 'vue'
import type { Card } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import type { Message } from '@/arkham/types/Message'
import { MessageType} from '@/arkham/types/Message'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as ArkhamGame from '@/arkham/types/Game'

const props = defineProps<{
  game: Game
  card: Card
  investigatorId: string
}>()

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
    case MessageType.PLAY_CARD_AS:
      return c.contents[1] === id.value
    case MessageType.PLAY_DYNAMIC_CARD:
      return c.contents[1] === id.value
    case MessageType.PLAY_FAST_EVENT:
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
    case MessageType.TARGET_LABEL:
      return c.contents[0].contents === id.value
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

function isActivate(v: Message) {
  if (v.tag !== 'UseAbility') {
    return false
  }

  const { contents } = v.contents[1].source;

  if (contents === id.value) {
    return true
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<number[]>((acc, v, i) => {
      if (v.tag === 'Run' && isActivate(v.contents[0])) {
        return [...acc, i];
      } else if (isActivate(v)) {
        return [...acc, i];
      }

      return acc;
    }, []);
})

const classObject = computed(() => {
  return {
    'card--can-interact': cardAction.value !== -1,
    'card--committed': uncommitCardAction.value !== -1,
  }
})

const baseUrl = inject('baseUrl')

const image = computed(() => {
  const { cardCode } = props.card.contents;
  return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}.jpg`;
})
</script>

<template>
  <div class="card-container">
    <img
      :class="classObject"
      class="card"
      :src="image"
      @click="$emit('choose', cardAction)"
    />

    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      :data-image="image"
      @click="$emit('choose', ability)"
      />

  </div>
</template>

<style scoped lang="scss">

.card {
  width: $card-width;
  min-width: $card-width;
  border-radius: 7px;
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  display: flex;

  &--can-interact {
    border: 2px solid $select;
    cursor: pointer;
  }

  &--committed {
    margin-top: -10px;
  }
}
</style>
