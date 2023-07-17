<script lang="ts" setup>
import type { Game } from '@/arkham/types/Game';
import type { EncounterCardContents } from '@/arkham/types/Card';
import { imgsrc } from '@/arkham/helpers'

export interface Props {
  game: Game
  cards: EncounterCardContents[]
}

const props = defineProps<Props>()
const image = (card: EncounterCardContents) => {
  const { cardCode } = card;
  return imgsrc(`cards/${cardCode.replace('c', '')}.jpg`);
}
</script>

<template>
  <div class="discards">
    <div v-for="card in cards" :key="card.id" class="discard">
      <img
        class="card"
        :src="image(card)"
      />
    </div>
  </div>
</template>

<style scoped lang="scss">
.discards {
  background: white;
  display: flex;
  align-items: center;
  padding: 10px 0;
  width: 100%;
  overflow-x: auto;
}

.discard {
  padding-left: 10px;
  position: relative;
  width: 110px;
  &::after {
    pointer-events: none;
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: #FFF;
    opacity: .85;
    mix-blend-mode: saturation;
  }
}
</style>
