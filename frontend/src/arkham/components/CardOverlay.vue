<template>
  <div v-if="card" class="card-overlay" :style="{top, left}">
    <img :src="card" />
  </div>
</template>

<script lang="ts">
import { defineComponent, ref, computed } from 'vue';

export default defineComponent({
  setup() {
    const card = ref<string | null>(null);
    const topPosition = ref(50)
    const leftPosition = ref(10)

    const top = computed(() => topPosition.value + 'px')
    const left = computed(() => leftPosition.value + 'px')

    document.addEventListener('mousemove', (event) => {
      if (event.target instanceof HTMLImageElement) {
        if (event.target.classList.contains('card')) {
          const clientRect = event.target.getBoundingClientRect ();
          topPosition.value = Math.max(clientRect.top - 300, 50)
          leftPosition.value = Math.min(clientRect.left + 150, window.innerWidth - 300)
          card.value = event.target.src
        } else {
          card.value = null
        }

      } else if (event.target instanceof HTMLDivElement) {
        if (event.target.classList.contains('card')) {
          const clientRect = event.target.getBoundingClientRect ();
          topPosition.value = Math.max(clientRect.top - 300, 50)
          leftPosition.value = Math.min(clientRect.left + 150, window.innerWidth - 300)
          card.value = event.target.style.backgroundImage.slice(4, -1).replace(/"/g, "")
        } else {
          card.value = null
        }
      } else {
        card.value = null
      }
    })

    return { card, top, left }
  }
})
</script>

<style lang="scss">
.card-overlay {
  position: fixed;
  width: 100px;
  img {
    border-radius: 20px;
    width: 250px;
  }
  pointer-events: none;
  z-index: 10000;
}
</style>
