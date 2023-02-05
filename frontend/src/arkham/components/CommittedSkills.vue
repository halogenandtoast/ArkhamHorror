<script lang="ts" setup>
import type { Game } from '@/arkham/types/Game';
import type { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';

export interface Props {
  game: Game
  cards: CardContents[]
  investigatorId: string
}

const props = defineProps<Props>()
</script>

<template>
  <div class="card-row-cards">
    <div v-for="card in props.cards" :key="card.id" class="card-row-card">
      <Card :game="props.game" :card="card" :investigatorId="props.investigatorId" @choose="$emit('choose', $event)" />
    </div>
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
  }

  h2 {
    font-size: 1.8em;
    color: rgba(255,255,255,0.5);
    padding: 0;
    margin: 0;
  }
}

.card-row-cards {
  display: flex;
  align-items: center;
  justify-content: center;
  padding-bottom: 10px;
  flex-wrap: wrap;
}

.card-row-card {
  margin-left: 10px;
  position: relative;
}

.card {
  width: $card-width;
  border-radius: 6px;
  margin: 2px;
}

.skill--can-interact {
  border: 3px solid $select;
  cursor: pointer;
}
</style>
