<script lang="ts" setup>
import type { Game } from '@/arkham/types/Game';
import type { PlayerCard } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';

export interface Props {
  game: Game
  investigatorId: string
  cards: PlayerCard[]
}

defineProps<Props>()
</script>

<template>
  <div class="discards">
    <div v-for="card in cards" :key="card.id" class="discard">
      <Card :game="game" :card="card" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
    </div>
  </div>
</template>

<style scoped lang="scss">
.discards {
  background: #759686;
  display: flex;
  align-items: center;
  padding: 10px 0;
  width: 100%;
  overflow-x: auto;
}

.discard {
  margin-left: 10px;
  position: relative;
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
