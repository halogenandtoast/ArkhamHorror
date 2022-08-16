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

const cardAction = computed(() => {
  return choices.value.findIndex((choice) => {
    if (choice.tag === MessageType.TARGET_LABEL) {
      return choice.target.tag === "CardIdTarget" && choice.target.contents === id.value
    }

    return false
  })
})

function isActivate(v: Message) {
  // if (v) {
  //   if (v.tag !== 'AbilityLabel') {
  //     return false
  //   }
  //
  //   const { contents } = v.contents[1].source;
  //
  //   if (contents === id.value) {
  //     return true
  //   }
  // }

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
    // 'card--committed': uncommitCardAction.value !== -1,
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
