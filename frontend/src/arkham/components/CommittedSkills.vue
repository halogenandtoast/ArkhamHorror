<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Card, CardContents } from '@/arkham/types/Card';
import CardView from '@/arkham/components/Card.vue';
import Skill from '@/arkham/components/Skill.vue';

const props = defineProps<{
  game: Game
  cards: (Card | CardContents)[]
  playerId: string
}>()

const cardContents = computed<CardContents[]>(() => {
  return props.cards.map<CardContents>(c => c.tag === 'CardContents' ? c :
    (c.tag === 'VengeanceCard' ? c.contents.contents : c.contents)).reverse()
})

const emits = defineEmits<{
  choose: [value: number]
}>()

const choose = (value: number) => {
  emits('choose', value)
}

function skillId(card: Card) {
  const skill = Object.values(props.game.skills).find(s => s.cardId === card.id)
  if (skill) {
    return skill.id
  }

  return null
}
</script>

<template>
  <div class="card-row-cards">
    <div class="card-row-cards--inner">
      <div v-for="card in cardContents" :key="card.id" class="card-row-card">
        <Skill v-if="skillId(card)" :game="props.game" :skill="game.skills[skillId(card)]" :playerId="playerId" @choose="choose" />
        <CardView v-else :game="game" :card="card" :playerId="playerId" @choose="choose" />
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
