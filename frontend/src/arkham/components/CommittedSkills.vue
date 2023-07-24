<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';

const props = defineProps<{
  game: Game
  cards: CardContents[]
  investigatorId: string
}>()

const emits = defineEmits<{
  choose: [value: number]
}>()

const choose = (value: number) => {
  emits('choose', value)
}

const cards = computed(() => props.cards.filter(c => c).reverse())
</script>

<template>
  <div class="card-row-cards">
    <div class="card-row-cards--inner">
      <div v-for="card in cards" :key="card.id" class="card-row-card">
        <Card :game="props.game" :card="card" :investigatorId="props.investigatorId" @choose="choose" />
      </div>
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
  position: relative;
  height: $card-width * 1.4;
  padding-bottom: 10px;
}

.card-row-cards--inner {
  display: flex;
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
