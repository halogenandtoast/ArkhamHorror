<template>
  <div class="card-row">
    <header>
      <h2>{{title}}</h2>
    </header>
    <div class="card-row-cards">
      <div v-for="card in cards" :key="card.id" class="card-row-card" :class="{ discard: isDiscards }">
        <Card :game="game" :card="card" :investigatorId="investigatorId" @choose="$emit('choose', $event)" />
      </div>
    </div>
    <button class="close" @click="$emit('close')">Close</button>
  </div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';
import { Game } from '@/arkham/types/Game';
import { CardContents } from '@/arkham/types/Card';
import Card from '@/arkham/components/Card.vue';

export default defineComponent({
  components: {
    Card,
  },
  props: {
    game: { type: Object as () => Game, required: true },
    cards: { type: Array as () => CardContents[], required: true },
    investigatorId: { type: String, required: true },
    isDiscards: { type: Boolean, default: false },
    title: { type: String, required: true }
  },
  setup() {
    const image = (card: CardContents) => {
      const { cardCode, isFlipped } = card
      const suffix = isFlipped === true ? 'b' : ''
      const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : ''
      return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}${suffix}.jpg`;
    }

    return { image }
  }
})
</script>

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
}

.card-row-card {
  margin-left: 10px;
  position: relative;
}

.discard {
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
</style>
