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

<script lang="ts">
import { defineComponent } from 'vue';
import { Game } from '@/arkham/types/Game';
import { PlayerCardContents } from '@/arkham/types/Card';

export default defineComponent({
  props: {
    game: { type: Object as () => Game, required: true },
    investigatorId: { type: String, required: true },
    cards: { type: Array as () => PlayerCardContents[], required: true }
  },
  setup() {
    const image = (card: PlayerCardContents) => {
      const { cardCode } = card.def;
      const baseUrl = process.env.NODE_ENV == 'production' ? process.env.VUE_APP_ASSET_HOST : '';
      return `${baseUrl}/img/arkham/cards/${cardCode}.jpg`;
    }

    return { image }
  }
})
</script>

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
    /* background-image: linear-gradient(120deg, #eaee44, #33d0ff); */
    opacity: .85;
    mix-blend-mode: saturation;
  }
}
</style>
