<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Token from '@/arkham/components/Token.vue'
import * as Arkham from '@/arkham/types/Story'
import PoolItem from '@/arkham/components/PoolItem.vue';
import { TokenType } from '@/arkham/types/Token';

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
  const { art, flippedArt, flipped } = props.story
  const storyArt = flipped ? flippedArt : art
  return imgsrc(`cards/${storyArt.replace(/^c/, '')}.avif`);
})

const id = computed(() => props.story.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choose = (idx: number) => emit('choose', idx)

const checkmarks = computed(() => {
  return props.story.modifiers?.filter(m =>
    m.type.tag === 'UIModifier' &&
    m.type.contents.tag === 'OverlayCheckmark'
  ).map(m => m.type.contents) ?? []
})


const setAsideInfestationTokens = computed(() => props.story.meta?.infestationSetAside ?? [])

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

const crossedOff = computed(() => {
  const entries = props.story.meta?.crossedOff
  if (!entries) return null
  return JSON.stringify(entries)
})

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

const clues = computed(() => props.story.tokens[TokenType.Clue])
const horror = computed(() => props.story.tokens[TokenType.Horror])
const civilians = computed(() => props.story.tokens[TokenType.Civilian])

const hasPool = computed(() => {
  return (clues.value && clues.value > 0) || (horror.value && horror.value > 0)
})
</script>

<template>
  <div class="story">
    <div class="story-card">
      <div class="image-container">
        <img :src="image"
          :class="{'story--can-interact': cardAction !== -1 }"
          :data-crossed-off="crossedOff"
          :data-checkmarks="JSON.stringify(checkmarks)"
          class="card story"
          @click="$emit('choose', cardAction)"
        />
        <div class="pool" v-if="hasPool">
          <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
          <PoolItem v-if="horror && horror > 0" type="horror" :amount="horror" />
        </div>
        <PoolItem class="civilians" v-if="civilians" type="resource" :amount="civilians" />
      </div>
      <AbilityButton
        v-for="ability in abilities"
        :key="ability.index"
        :ability="ability.contents"
        :data-image="image"
        :game="game"
        @click="$emit('choose', ability.index)"
        />
    </div>
    <div v-if="setAsideInfestationTokens.length > 0" class="infestation-tokens">
      <Token v-for="token in setAsideInfestationTokens" :key="token.id" :token="Arkham.infestationAsChaosToken(token)" :playerId="playerId" :game="game" @choose="choose" />
    </div>
      
  </div>
</template>

<style scoped>
.story--can-interact {
  border: 3px solid var(--select);
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

.image-container {
  position: relative;
  isolation: isolate;
  display: grid;
  grid-template-areas: "base";
  place-items: center;
  place-content: center;

  > .pool {
    grid-area: base;
    pointer-events: none;
  }

  > .card {
    grid-area: base;
  }
}

.civilians {
  position: absolute;
  bottom: 0;
  right: 0;
  pointer-events: none;
  z-index: 10;
}
</style>
