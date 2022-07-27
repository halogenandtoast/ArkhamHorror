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
  position: relative;
  img {
    position: absolute;
    object-fit: contain;
    border-radius: 50px;
    height: 100%;
    width: calc(100% - 20px);
    top: 0px;
    left: 10px;
    margin: 0 auto;
  }
}
</style>
