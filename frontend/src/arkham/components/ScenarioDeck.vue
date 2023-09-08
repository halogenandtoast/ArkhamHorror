<script lang="ts" setup>
import { computed } from 'vue';
import type { Card } from '@/arkham/types/Card';
import { imgsrc } from '@/arkham/helpers';

export interface Props {
  deck: [string, Card[]]
}

const props = defineProps<Props>()

const deckImage = computed(() => {
  switch(props.deck[0]) {
    case 'UnknownPlacesDeck':
      return imgsrc("cards/05134b.jpg");
    case 'ExhibitDeck':
      return imgsrc("cards/02132b.jpg");
    case 'CosmosDeck':
      return imgsrc("cards/05333b.jpg");
    case 'CatacombsDeck':
      return imgsrc("cards/03247b.jpg");
    default:
      return imgsrc("back.png");
  }
})

const deckLabel = computed(() => {
  switch(props.deck[0]) {
    case 'CultistDeck':
      return "Cultists"
    case 'LunaticsDeck':
      return "Lunatics"
    case 'MonstersDeck':
      return "Monsters"
    default:
      return null
  }
})
</script>

<template>
  <div class="deck">
    <img
      :src="deckImage"
      class="card"
    />
    <span v-if="deckLabel" class="deck-label">{{deckLabel}}</span>
    <span class="deck-size">{{deck[1].length}}</span>
  </div>
</template>

<style scoped lang="scss">
.card {
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
  border-radius: 6px;
  margin: 2px;
  width: $card-width;
}

.deck {
  position: relative;
}

.deck-label {
  position: absolute;
  top: 0;
  left: 50%;
  font-weight: bold;
  border-radius: 3px;
  padding: 0 2px;
  transform: translateX(-50%) translateY(50%);
  background: rgba(255,255,255,0.8);
}

.deck-size {
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  width: 1.3em;
  height: 1.3em;
  border-radius: 1.3em;
  text-align: center;
  color: rgba(255, 255, 255, 0.7);
  background-color: rgba(0, 0, 0, 0.8);
  left: 50%;
  bottom: 0%;
  transform: translateX(-50%) translateY(-50%);
  pointer-events: none;
}
</style>
