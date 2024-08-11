<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Token from '@/arkham/components/Token.vue'
import * as Arkham from '@/arkham/types/Story'

export interface Props {
  game: Game
  story: Arkham.Story
  playerId: string
  atLocation?: boolean
}

const props = withDefaults(defineProps<Props>(), { atLocation: false })
const emit = defineEmits<{
  choose: [value: number]
}>()

const image = computed(() => {
  const { id, flipped } = props.story
  const suffix = flipped ? 'b' : ''
  return imgsrc(`cards/${id.replace('c', '')}${suffix}.jpg`);
})

const id = computed(() => props.story.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choose = (idx: number) => emit('choose', idx)

const setAsideInfestationTokens = computed(() => props.story.meta?.infestationSetAside ?? [])

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
    <div class="story-card">
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
    <div v-if="setAsideInfestationTokens.length > 0" class="infestation-tokens">
      <Token v-for="token in setAsideInfestationTokens" :key="token.id" :token="Arkham.infestationAsChaosToken(token)" :playerId="playerId" :game="game" @choose="choose" />
    </div>
      
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
  flex-direction: row;

  & :deep(.token) {
    width: 2em;
  }

  & :deep(.token-container) {
    width: fit-content;
  }
}

.infestation-tokens {
  width: fit-content;
  display: grid;
  grid-auto-flow: column;
  grid-template-rows: 2em 2em;
  gap: 5px;
  padding: 5px;
  margin: 5px;
  background: rgba(255, 255, 255, 0.3);
  border: 1px solid rgba(255, 255, 255, 0.6);
  border-radius: 5px;
  height: calc(4em + 10px);
}

.story-card {
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
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
}
</style>
