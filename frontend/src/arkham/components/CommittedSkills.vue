<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';
import CardStack from '@/arkham/components/CardStack.vue';

export interface Props {
  game: Game
  cards: CardContents[]
  investigatorId: string
}

const props = defineProps<Props>()
const cards = computed(() => props.cards.filter(c => c).reverse())
</script>

<template>
  <CardStack>
    <Card v-for="card in cards" :key="card.id" :game="props.game" :card="card" :investigatorId="props.investigatorId" @choose="$emit('choose', $event)" />
  </CardStack>
</template>
