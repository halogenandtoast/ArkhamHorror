<script lang="ts" setup>
import { withDefaults, computed, inject } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { Message, MessageType } from '@/arkham/types/Message'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Story'

export interface Props {
  game: Game
  story: Arkham.Story
  investigatorId: string
  atLocation?: boolean
}

const props = withDefaults(defineProps<Props>(), { atLocation: false })
const baseUrl = inject('baseUrl')

const image = computed(() => {
  const { id } = props.story
  return `${baseUrl}/img/arkham/cards/${id.replace('c', '')}.jpg`;
})

const id = computed(() => props.story.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

function isAbility(v: Message) {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { tag } = v.ability.source;

  if (tag === 'ProxySource') {
    return v.ability.source.source.contents === id.value
  } else if (tag === 'StorySource') {
    return v.ability.source.contents === id.value
  }

  return false
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
  <div class="story">
    <img :src="image"
      :class="{'story--can-interact': cardAction !== -1 }"
      class="card story"
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
.story--can-interact {
  border: 3px solid $select;
  border-radius: 15px;
  cursor: pointer;
}

.story {
  display: flex;
  flex-direction: column;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
}
</style>
