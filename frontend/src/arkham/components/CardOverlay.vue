<script lang="ts" setup>
import { ref } from 'vue';
import { imgsrc } from '@/arkham/helpers'

const card = ref<string | null>(null);
const cardOverlay = ref<HTMLElement | null>(null);

const getPosition = (el: HTMLElement) => {
  const rect = el.getBoundingClientRect()

  // we do not know the height of the overlay, BUT we can calculate it from the width and height of the target.
  // since we know the overlay's width is 300px we get the ration and multiply the height
  // afterwards we add this new height to it's top to figure out if we are off the screen. If we are we use the
  // bottom value instead

  const ratio = rect.width / rect.height
  const height = 300 / ratio

  const top = rect.top + window.scrollY - 40

  const bottom = top + height

  const newTop = bottom > window.innerHeight ?
    (top + rect.height) - height - 10 :
    top

  return { top: newTop, left: rect.left + window.scrollX + rect.width + 10 }
}

const getImage = (el: HTMLElement): string | null => {
  if (el instanceof HTMLImageElement) {
    if (el.classList.contains('card')) {
      return el.src
    }
  } else if (el instanceof HTMLDivElement) {
    if (el.classList.contains('card')) {
      return el.style.backgroundImage.slice(4, -1).replace(/"/g, "")
    }
  } else if (el instanceof HTMLElement) {
    if(el.dataset.imageId) {
      return imgsrc(`cards/${el.dataset.imageId}.jpg`)
    }
    if(el.dataset.target) {
      const target = document.querySelector(`[data-id="${el.dataset.target}"]`) as HTMLElement
      if (target !== null) {
        return getImage(target)
      }
    }
    if(el.dataset.image) {
      return el.dataset.image
    }
  }

  return null
}

document.addEventListener('mouseover', (event) => {
  if (cardOverlay.value === null) {
    return
  }

  card.value = getImage(event.target as HTMLElement)

  if (event.target instanceof HTMLImageElement) {
    if (event.target.classList.contains('card')) {
      const { top, left } = getPosition(event.target)

      cardOverlay.value.style.top = `${top}px`
      cardOverlay.value.style.left = `${left}px`

      return
    }
  } else if (event.target instanceof HTMLDivElement) {
    if (event.target.classList.contains('card')) {
      const { top, left } = getPosition(event.target)

      cardOverlay.value.style.top = `${top}px`
      cardOverlay.value.style.left = `${left}px`
      return
    }
  } else if (event.target instanceof HTMLElement) {
    if(event.target.dataset.imageId) {
      const { top, left } = getPosition(event.target)

      cardOverlay.value.style.top = `${top}px`
      cardOverlay.value.style.left = `${left}px`
      return
    }
    if(event.target.dataset.target) {
      const target = document.querySelector(`[data-id="${event.target.dataset.target}"]`) as HTMLElement
      if (target === null) {
        return
      }
      const { top, left } = getPosition(target)

      cardOverlay.value.style.top = `${top}px`
      cardOverlay.value.style.left = `${left}px`
      return
    }
    if(event.target.dataset.image) {
      const { top, left } = getPosition(event.target)

      cardOverlay.value.style.top = `${top}px`
      cardOverlay.value.style.left = `${left}px`
      return
    }
  }
})
</script>

<template>
  <div class="card-overlay" ref="cardOverlay">
    <img v-if="card" :src="card" />
  </div>
</template>

<style lang="scss">
.card-overlay {
  position: absolute;
  z-index: 1000;
  width: 300px;
  height: fit-content;
  display: flex;
  img {
    object-fit: contain;
    border-radius: 15px;
    width: auto;
    height: auto;
    max-width: 100%;
    max-height: 100%;
  }
}
</style>
