<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/Story'

export interface Props {
  game: Game
  story: Arkham.Story
  playerId: string
  atLocation?: boolean
}

const props = withDefaults(defineProps<Props>(), { atLocation: false })

const image = computed(() => {
  const { id, flipped } = props.story
  const suffix = flipped ? 'b' : ''
  return imgsrc(`cards/${id.replace('c', '')}${suffix}.jpg`);
})

const id = computed(() => props.story.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'StorySource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i}];
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
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="$emit('choose', ability.index)"
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
