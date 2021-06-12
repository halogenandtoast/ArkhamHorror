<template>
  <div class="card-overlay">
    <img v-if="card" :src="card" />
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue';

export default defineComponent({
  setup() {
    const card = ref<string | null>(null);

    document.addEventListener('mousemove', (event) => {
      if (event.target instanceof HTMLImageElement) {
        if (event.target.classList.contains('card')) {
          card.value = event.target.src
        }
      } else if (event.target instanceof HTMLDivElement) {
        if (event.target.classList.contains('card')) {
          card.value = event.target.style.backgroundImage.slice(4, -1).replace(/"/g, "")
        }
      }
    })

    return { card }
  }
})
</script>

<style lang="scss">
.card-overlay {
  width: 100%;
  height: 25%;
  img {
    border-radius: 20px;
    height: 100%;
    width: 100%;
    object-fit: contain;
    margin: 0 auto;
  }
  pointer-events: none;
}
</style>
