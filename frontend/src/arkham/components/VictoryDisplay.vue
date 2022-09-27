<script lang="ts" setup>
import { computed, ref, inject } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Card } from '@/arkham/types/Card';

export interface Props {
  game: Game
  victoryDisplay: Card[]
}

const props = defineProps<Props>()
const emit = defineEmits(['show'])
const reference = computed(() => ref(props.victoryDisplay))

const baseUrl = inject('baseUrl')
const topOfVictoryDisplay = computed(() => {
  if (props.victoryDisplay[0]) {
    const { cardCode } = props.victoryDisplay[0].contents;
    return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}.jpg`;
  }

  return null;
})

const viewVictoryDisplayLabel = computed(() => `${props.victoryDisplay.length} Cards`)

const showVictoryDisplay = (e: Event) => emit('show', e, reference.value, 'Victory Display', true)
</script>

<template>
  <div v-if="topOfVictoryDisplay" class="victory-display">
    <img
      :src="topOfVictoryDisplay"
      class="card"
    />

    <button @click="showVictoryDisplay">{{viewVictoryDisplayLabel}}</button>
  </div>
</template>

<style scoped lang="scss">
.card {
  width: 100px;
  border-radius: 6px;
  margin: 2px;
}

.victory-display {
  height: 100%;
  position: relative;
  display: flex;
  flex-direction: column;
  &::after {
    pointer-events: none;
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: #FFF;
    /* background-image: linear-gradient(120deg, #eaee44, #33d0ff); */
    opacity: .85;
    mix-blend-mode: saturation;
  }
}
</style>
