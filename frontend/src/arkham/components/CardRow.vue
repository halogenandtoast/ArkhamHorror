<script lang="ts" setup>
import type { Game } from '@/arkham/types/Game';
import type { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';
import Draggable from '@/components/Draggable.vue';

withDefaults(defineProps<{
  game: Game
  cards: CardContents[]
  playerId: string
  isDiscards?: boolean
  title: string
}>(), { isDiscards: false })
</script>

<template>
  <Draggable class="card-row">
    <template #handle>
      <h2>{{title}}</h2>
    </template>
    <div class="card-row-cards">
      <div v-for="card in cards" :key="card.id" class="card-row-card" :class="{ discard: isDiscards }">
        <Card :game="game" :card="card" :playerId="playerId" @choose="$emit('choose', $event)" />
      </div>
    </div>
    <button class="close" @click="$emit('close')">Close</button>
  </Draggable>
</template>

<style scoped lang="scss">
.card-row {
  background: #759686;
  width: 100%;
  overflow-x: auto;
  text-align: center;
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  gap: 2px;
  flex-wrap: wrap;
}

.card-row-card {
  position: relative;
}

.discard {
  filter: grayscale(0.85);
}

.card {
  width: $card-width;
  border-radius: 6px;
  margin: 2px;
}

button {
  border: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #4d2b61;
  }
  margin-bottom: 10px;
}

.card-row {
  position: absolute;
  width: 80%;
  top: 50%;
  left: 50%;
  background: hsl(150.9 13.6% 52.4% / 80%);
  transform: translateX(-50%) translateY(-50%);

  background: rgba(94,123,115,0.5);
  border-radius: 16px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  z-index: 1000000;
}
</style>
