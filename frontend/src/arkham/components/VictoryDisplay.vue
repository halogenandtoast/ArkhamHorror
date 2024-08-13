<script lang="ts" setup>
import { computed, ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Card } from '@/arkham/types/Card';
import CardView from '@/arkham/components/Card.vue'

export interface Props {
  game: Game
  playerId: string
  victoryDisplay: Card[]
}

const props = defineProps<Props>()
const emit = defineEmits(['show'])
const reference = ref(props.victoryDisplay)
const topOfVictoryDisplay = computed(() => props.victoryDisplay[0])

const viewVictoryDisplayLabel = computed(() => `${props.victoryDisplay.length} Card${props.victoryDisplay.length === 1 ? '' : 's'}`)

const showVictoryDisplay = () => emit('show', reference, 'Victory Display', true)
</script>

<template>
  <div v-if="topOfVictoryDisplay" class="victory-display">
    <div class="victory-display-card">
      <CardView :game="game" :card="topOfVictoryDisplay" :playerId="playerId" />
    </div>

    <button @click="showVictoryDisplay">{{viewVictoryDisplayLabel}}</button>
  </div>
</template>

<style scoped lang="scss">
.card {
  width: 100px;
  border-radius: 6px;
  box-shadow: unset;
}

.victory-display {
  display: flex;
  flex-direction: column;
  gap: 5px;

  &:deep(.card) {
    box-shadow: unset;
    margin: 0;
  }
}

.victory-display-card {
  position: relative;
  width: fit-content;
  line-height: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);

  &::after {
    border-radius: 6px;
    pointer-events: none;
    content: "";
    position: absolute;
    inset: 0;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
}
</style>
