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

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Card } from '@/arkham/types/Card';

export default defineComponent({
  props: {
    deck: { type: Object as () => [string, Card[]], required: true }
  },
  setup(props) {
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';

    const deckImage = computed(() => {
      switch(props.deck[0]) {
        case 'ExhibitDeck':
          return `${baseUrl}/img/arkham/cards/02132b.jpg`;
        case 'CatacombsDeck':
          return `${baseUrl}/img/arkham/cards/03247b.jpg`;
        default:
          return `${baseUrl}/img/arkham/back.png`;
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

    return { deckImage, deckLabel }
  }
})
</script>

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
