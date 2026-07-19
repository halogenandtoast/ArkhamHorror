import { onBeforeUnmount, ref, watch, type Ref } from 'vue'

export function useCardFlip(image: Readonly<Ref<string>>) {
  const displayedImage = ref(image.value)
  const flipping = ref(false)
  let imageSwapTimer: number | undefined
  let animationTimer: number | undefined

  watch(image, (nextImage) => {
    window.clearTimeout(imageSwapTimer)
    window.clearTimeout(animationTimer)

    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
      displayedImage.value = nextImage
      flipping.value = false
      return
    }

    flipping.value = true
    imageSwapTimer = window.setTimeout(() => {
      displayedImage.value = nextImage
    }, 225)
    animationTimer = window.setTimeout(() => {
      flipping.value = false
    }, 450)
  })

  onBeforeUnmount(() => {
    window.clearTimeout(imageSwapTimer)
    window.clearTimeout(animationTimer)
  })

  return { displayedImage, flipping }
}
