<template>
  <div v-if="card" class="card-overlay">
    <img :src="card" />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';

@Component
export default class CardOverlay extends Vue {
  card: string | null = null

  mounted() {
    document.addEventListener('mousemove', (event) => {
      if (event.target instanceof HTMLImageElement) {
        if (event.target.classList.contains('card')) {
          this.card = event.target.src;
        } else {
          this.card = null;
        }
      } else {
        this.card = null;
      }
    });
  }
}
</script>

<style lang="scss">
.card-overlay {
  position: fixed;
  top: 50px;
  right: 10px;
  img {
    border-radius: 20px;
  }
  pointer-events: none;
  z-index: 10000;
}
</style>
