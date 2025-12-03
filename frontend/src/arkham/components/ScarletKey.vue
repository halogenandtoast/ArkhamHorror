<script lang="ts" setup>
import { computed } from 'vue'
import { Game } from '@/arkham/types/Game'
import * as ArkhamGame from '@/arkham/types/Game'
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message'
import { imgsrc } from '@/arkham/helpers'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import * as Arkham from '@/arkham/types/ScarletKey'
import PoolItem from '@/arkham/components/PoolItem.vue';
import { TokenType } from '@/arkham/types/Token';

export interface Props {
  game: Game
  scarletKey: Arkham.ScarletKey
  playerId: string
  atLocation?: boolean
}

const props = withDefaults(defineProps<Props>(), { atLocation: false })
const emit = defineEmits<{
  choose: [value: number]
}>()

const image = computed(() => {
  const { id, stability } = props.scarletKey
  const suffix = stability == 'Unstable' ? 'b' : ''
  return imgsrc(`cards/${id.replace('c', '')}${suffix}.avif`);
})

const id = computed(() => props.scarletKey.id)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choose = (idx: number) => emit('choose', idx)

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

const crossedOff = computed(() => {
  const entries = props.scarletKey.meta?.crossedOff
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
  } else if (source.tag === 'ScarletKeySource') {
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

const clues = computed(() => props.scarletKey.tokens[TokenType.Clue])
const resources = computed(() => props.scarletKey.tokens[TokenType.Resource])
</script>

<template>
  <div class="scarletKey">
    <div class="scarletKey-card">
      <div class="image-container">
        <img :src="image"
          :class="{'scarletKey--can-interact': cardAction !== -1 }"
          :data-crossed-off="crossedOff"
          class="card scarletKey"
          @click="$emit('choose', cardAction)"
        />
        <div class="pool">
          <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
          <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
        </div>
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
  </div>
</template>

<style scoped>
.scarletKey--can-interact {
  border: 3px solid var(--select);
  border-radius: 15px;
  cursor: pointer;
}

.scarletKey {
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

.scarletKey-card {
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
}

.pool {
  position: absolute;
  top: 10%;
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  align-self: flex-start;
  align-items: flex-end;
  z-index: 15;
  :deep(img) {
    width: 20px;
    height: auto;
  }

  :deep(.token-container) {
    width: 20px;
  }

  &:not(:has(.key--can-interact)) {
    pointer-events: none;
  }
}
</style>
