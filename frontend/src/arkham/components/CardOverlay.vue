<script lang="ts" setup>
import { ref, inject } from 'vue';

const card = ref<string | null>(null);
const baseUrl = inject('baseUrl')

document.addEventListener('mouseover', (event) => {
  if (event.target instanceof HTMLImageElement) {
    if (event.target.classList.contains('card')) {
      card.value = event.target.src
    }
  } else if (event.target instanceof HTMLDivElement) {
    if (event.target.classList.contains('card')) {
      card.value = event.target.style.backgroundImage.slice(4, -1).replace(/"/g, "")
    }
  } else if (event.target instanceof HTMLElement) {
    if(event.target.dataset.imageId) {
      card.value = `${baseUrl}/img/arkham/cards/${event.target.dataset.imageId}.jpg`
    }
    if(event.target.dataset.image) {
      card.value = event.target.dataset.image
    }
  }
})
</script>

<template>
  <div class="card-overlay">
    <img v-if="card" :src="card" />
  </div>
</template>

<style lang="scss">
.card-overlay {
  width: 100%;
  flex: 1 1 50%;
  padding: 10px;
  box-sizing: border-box;
  display: flex;
  height: 450px;
  align-items: center;
  justify-items: center;
  justify-content: center;
  align-content: center;
  img {
    object-fit: contain;
    border-radius: 15px;
    width: auto;
    height: auto;
    max-width: 100%;
    max-height: 450px;
  }
}
</style>
