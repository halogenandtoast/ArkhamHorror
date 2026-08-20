<script lang="ts" setup>
import { useAttrs, inject, ref, computed, watch, type Ref } from 'vue'
import { altFrontImage, cardBackImage, cardFrontImage, hasCardBackArt } from '@/arkham/cardArt'
import { CardDef } from '@/arkham/types/CardDef'
import { ArrowPathIcon } from '@heroicons/vue/20/solid'


defineOptions({
  inheritAttrs: false
})

const attrs = useAttrs()

const props = defineProps<{ card: CardDef }>()

// An ancestor (e.g. the card browser) can provide a shared flip state to flip
// every card it renders at once. Individual flips still work on top of it, and
// cards rendered later (a new filter) start on the side everything else is on.
const flipAll = inject<Ref<boolean> | null>('cardFlipAll', null)

const wantsFlip = ref(flipAll?.value ?? false)

if (flipAll) watch(flipAll, (value) => { wantsFlip.value = value })

const image = computed(() => cardFrontImage(props.card))
const backImage = computed(() => cardBackImage(props.card))

// Only offer the flip when the back is art of its own rather than a generic
// card back, and drop it again if that art turns out not to exist.
const backMissing = ref(false)
watch(backImage, () => { backMissing.value = false })

const flippable = computed(() => hasCardBackArt(props.card) && !backMissing.value)
const flipped = computed(() => wantsFlip.value && flippable.value)

// Some cards store their front art as an 'a' side; retry there once.
const frontSrc = ref(image.value)
watch(image, (src) => { frontSrc.value = src })

function onFrontError() {
  if (frontSrc.value !== image.value) return
  const alt = altFrontImage(image.value)
  if (alt) frontSrc.value = alt
}

// Full-height backs (an act/agenda that flips to an enemy or location) are stored
// portrait; act/agenda faces are landscape. Detect from the loaded back image
// instead of maintaining a card-code whitelist. Only matters once flipped.
const backVertical = ref(false)
function updateBackOrientation(e: Event) {
  const img = e.target as HTMLImageElement
  backVertical.value = img.naturalHeight > img.naturalWidth
}
watch(backImage, () => { backVertical.value = false })
const vertical = computed(() => flipped.value && backVertical.value)

</script>

<template>
  <div class='card-container' :class="[{vertical}]">
    <div class='front' :class="{flipped}">
      <img
        loading="lazy"
        :class="['card', 'card-front', { flipped }, attrs.class]"
        :src="frontSrc"
        @error="onFrontError"
        v-bind="attrs"
      />
      <button v-if="flippable" @click.prevent="wantsFlip = !wantsFlip"><ArrowPathIcon aria-hidden="true" /></button>
    </div>
    <div v-if="flippable" class="back" :class="{flipped}">
      <img
        loading="lazy"
        :class="['card', 'card-back', { flipped }, attrs.class]"
        :src="backImage"
        @load="updateBackOrientation"
        @error="backMissing = true"
        v-bind="attrs"
      />
      <button @click.prevent="wantsFlip = !wantsFlip"><ArrowPathIcon aria-hidden="true" /></button>
    </div>
  </div>
</template>

<style scoped>
.card {
  border-radius: 10px;
}
.card-container {
  overflow: hidden;
  width: fit-content;
  max-width: 250px;
  margin: 10px;
  border-radius: 10px;
  position: relative;

  &.vertical {
    aspect-ratio: var(--card-aspect);
  }

  button {
    border-radius: 2.4em;
    border-style: none;
    display: none;
    position: absolute;
    top: 5px;
    right: 5px;
    width: 2.4em;
    aspect-ratio: 1;
    background: gray;
    &:hover, &:focus {
      background: var(--neutral);
      color: var(--neutral-dark);
    }
    svg {
      position: absolute;
      inset: 15%;
    }
  }

  &:hover button {
    display: inherit;
  }
}

.front {
  width: 100%;
  backface-visibility: hidden;
  transition: transform 0.3s linear;
  perspective: 1000px;
  &.flipped {
    transform: rotateY(-180deg);
  }
}

.back {
  position:absolute;
  inset: 0;
  transform: rotateY(-180deg);
  transition: transform 0.3s linear;
  backface-visibility: hidden;

  &.flipped {
    transform: rotateY(0deg);
  }
}

@keyframes flip-back {
  0% {
    opacity: 1;
    transform: rotateY(0deg);
  }

  49% {
    opacity: 1;
  }

  50% {
    opacity: 0;
  }

  100% {
    transform: rotateY(-180deg);
    opacity: 0;
  }
}

@keyframes flip-front {
  0% {
    transform: rotateY(180deg);
    opacity: 0;
  }

  49% {
    opacity: 0;
  }

  50% {
    opacity: 1;
  }

  100% {
    opacity: 1;
    transform: rotateY(0deg);
  }

}
</style>
