<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';

export interface Props {
  game: Game
  cards: CardContents[]
  investigatorId: string
}

const props = defineProps<Props>()
const cards = computed(() => props.cards.filter(c => c).reverse())
</script>

<template>
  <div class="card-row-cards">
    <div class="card-row-cards--inner">
      <div v-for="card in cards" :key="card.id" class="card-row-card">
        <Card :game="props.game" :card="card" :investigatorId="props.investigatorId" @choose="$emit('choose', $event)" />
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
  width: calc($card-width + 20px);
  padding-bottom: 10px;
}

.card-row-cards--inner {
  display: grid;
  direction: rtl;
  grid-template-columns: repeat(auto-fit,  minmax(5px, max-content));
  &:hover {
    position: absolute;
    padding-right: 10px;
    background: rgba(0,0,0,0.2);
    display: flex;
    flex-direction: row;
    transform: translateX(-100%) translateX($card-width + 30px);
    .card-row-card {
      margin-left: 10px;
      width: $card-width;
      display: inline !important;
    }
  }
}

.card-row-card {
  position: relative;
  display: none;
  &:nth-of-type(n) {
    width: 5px;
  }

  &:nth-last-of-type(1) {
    display:inline;
  }

  &:nth-last-of-type(2) {
    display:inline;
  }

  &:nth-last-of-type(3) {
    display:inline;
  }
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
