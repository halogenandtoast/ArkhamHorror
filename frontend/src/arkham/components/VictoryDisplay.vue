<template>
  <div v-if="topOfVictoryDisplay" class="victory-display">
    <img
      :src="topOfVictoryDisplay"
      class="card"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';

export default defineComponent({
  props: { game: { type: Object as () => Game, required: true } },
  setup(props) {
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://arkham-horror-assets.s3.amazonaws.com" : '';
    const victoryDisplay = computed(() => props.game.victoryDisplay)
    const topOfVictoryDisplay = computed(() => {
      if (victoryDisplay.value[0]) {
        const { cardCode } = victoryDisplay.value[0].contents;
        return `${baseUrl}/img/arkham/cards/${cardCode}.jpg`;
      }

      return null;
    })

    return { topOfVictoryDisplay }
  }
})
</script>

<style lang="scss">
.card {
  width: 100px;
  border-radius: 6px;
}

.victory-display {
  height: 100%;
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
