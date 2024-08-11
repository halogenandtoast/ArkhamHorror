<script lang="ts" setup>

import Draggable from '@/components/Draggable.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import { computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import type { Game } from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Location';
import { imgsrc } from '@/arkham/helpers';
import { TokenType } from '@/arkham/types/Token';

type Props = {
  game: Game
  location: Arkham.Location
  playerId: string
}

const emit = defineEmits<{ close: [] }>()
const props = defineProps<Props>()

const debug = useDebug()
const id = computed(() => props.location.id)
const cardCode = computed(() => props.location.cardCode)
const image = computed(() => {
  return imgsrc(`cards/${cardCode.value.replace('c', '')}.jpg`)
})

const clues = computed(() => props.location.tokens[TokenType.Clue])

const hasPool = computed(() => {
  return clues.value > 0;
})

</script>

<template>
  <Draggable>
    <template #handle><h2>Debug Location</h2></template>
    <div class="location--outer">
      <div class="location" :data-index="location.cardId">
        <div class="card-frame">
          <div class="card-wrapper">
            <img :src="image" class="card-no-overlay" />
          </div>
          <div v-if="hasPool" class="pool">
            <PoolItem
              v-if="clues > 0"
              type="clue"
              :amount="clues"
            />
          </div>
        </div>
      </div>
      <div class="buttons">
        <button v-if="!location.revealed" @click="debug.send(game.id, {tag: 'RevealLocation', contents: [null, id]})">Reveal</button>
        <button v-if="clues && clues > 0" @click="debug.send(game.id, {tag: 'RemoveTokens', contents: [{ tag: 'TestSource', contents: []}, { tag: 'LocationTarget', contents: id }, 'Clue', clues]})">Remove Clues</button>
        <button @click="debug.send(game.id, {tag: 'PlaceTokens', contents: [{ tag: 'TestSource', contents: []}, { tag: 'LocationTarget', contents: id }, 'Clue', 1]})">Place Clue</button>
        <button @click="emit('close')">Close</button>
      </div>
    </div>
  </Draggable>
</template>

<style lang="scss" scoped>
.card-no-overlay {
  width: calc(var(--card-width) * 5); 
  max-width: calc(var(--card-width) * 5);
  border-radius: 15px;
  transform: rotate(0deg);
  transition: transform 0.2s linear;
}

.location {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.buttons {
  display: flex;
  flex-direction: column;
  justify-content: space-around;
  flex: 1;
  gap: 5px;
}

.location--outer {
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 10px;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.pool {
  position: absolute;
  top: 40%;
  align-items: center;
  width: 100%;
  display: flex;
  flex-wrap: wrap;
  :deep(.token-container) {
    width: unset;
  }
  :deep(img) {
    width: 20px;
    height: auto;
  }

  pointer-events: none;
}
</style>
