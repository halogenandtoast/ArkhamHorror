<template>
  <div v-if="topOfVictoryDisplay" class="victory-display">
    <img
      :src="topOfVictoryDisplay"
      class="card"
    />

    <button @click="showVictoryDisplay">{{viewVictoryDisplayLabel}}</button>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import { Card } from '@/arkham/types/Card';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    victoryDisplay: { type: Array as () => Card[], required: true }
  },
  setup(props, context) {
    const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
    const topOfVictoryDisplay = computed(() => {
      if (props.victoryDisplay[0]) {
        const { cardCode } = props.victoryDisplay[0].contents;
        return `${baseUrl}/img/arkham/cards/${cardCode.replace('c', '')}.jpg`;
      }

      return null;
    })

    const viewVictoryDisplayLabel = computed(() => `${props.victoryDisplay.length} Cards`)

    const showVictoryDisplay = (e: Event) => context.emit('show', e, props.victoryDisplay, 'Victory Display', true)

    return { topOfVictoryDisplay, viewVictoryDisplayLabel, showVictoryDisplay }
  }
})
</script>

<style scoped lang="scss">
.card {
  width: 100px;
  border-radius: 6px;
  margin: 2px;
}

.victory-display {
  height: 100%;
  position: relative;
  display: flex;
  flex-direction: column;
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
