<script lang="ts" setup>
import { withDefaults, computed, inject } from 'vue';
import type { Card } from '@/arkham/types/Card';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue'

export interface Props {
  game: Game
  card: Card
  revealed?: boolean
  investigatorId: string
}

const props = withDefaults(defineProps<Props>(), { revealed: false })
const emit = defineEmits(['choose'])
const baseUrl = inject('baseUrl')

const image = computed(() => {
  const { cardCode } = props.card.contents
  const suffix = !props.revealed && props.card.contents.isFlipped ? 'b' : ''
  return `${baseUrl}/img/arkham/cards/${cardCode.replace(/^c/, '')}${suffix}.jpg`
})

const id = computed(() => props.card.contents.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value
  }

  return false
}

const cardAction = computed(() => {
  return choices.value.findIndex(canInteract)
})


function isAbility(v: Message) {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { tag } = v.ability.source;

  if (tag === 'ProxySource') {
    return v.ability.source.source.contents === id.value
  } else {
    return v.ability.source.contents === id.value
  }
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<number[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, i];
      }

      return acc;
    }, []);
})
</script>

<template>
  <div class="card-container">
    <img
      :class="{'card--can-interact': cardAction !== -1}"
      class="card"
      :src="image"
      @click="emit('choose', cardAction)"
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
  display: inline-block;

  &--can-interact {
    border: 2px solid $select;
    cursor: pointer;
  }
}

.reaction-ability-button {
  background-color: #A02ECB;
  &:before {
    font-family: "arkham";
    content: "\0059";
    margin-right: 5px;
  }
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.card-container {
  display: flex;
  flex-direction: column;
}
</style>
