<template>
  <div v-if="card" class="card-overlay">
    <img :src="card" />
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
        } else {
          card.value = null
        }
      } else {
        card.value = null
      }
    })

    return { card }
  }
})
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
