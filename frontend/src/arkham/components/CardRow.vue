<script lang="ts" setup>
import { withDefaults } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';

export interface Props {
  game: Game
  cards: CardContents[]
  investigatorId: string
  isDiscards?: boolean
  title: string
}

const props = withDefaults(defineProps<Props>(), { isDiscards: false })
</script>

<template>
  <div class="card-row" draggable="true">
    <header>
      <h2>{{props.title}}</h2>
    </header>
    <div class="card-row-cards">
      <div v-for="card in props.cards" :key="card.id" class="card-row-card" :class="{ discard: isDiscards }">
        <Card :game="props.game" :card="card" :investigatorId="props.investigatorId" @choose="$emit('choose', $event)" />
      </div>
    </div>
    <button class="close" @click="$emit('close')">Close</button>
  </div>
</template>

<style scoped lang="scss">
.card-row {
  background: #759686;
  width: 100%;
  overflow-x: auto;
  text-align: center;

  header {
    padding: 10px;
    background: rgba(0 0 0 / 50%);
    border-bottom: 1px solid rgba(255 255 255 / 20%);
    text-transform: uppercase;
    font-size: 0.8em;
  }

  h2 {
    font-size: 1.8em;
    color: white;
    padding: 0;
    margin: 0;
  }
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  gap: 2px;
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
