<script lang="ts" setup>
import { useAttrs, inject, onBeforeUnmount, ref, computed, watch, type Ref } from 'vue'
import { altFrontImage, cardBackImage, cardFrontImage } from '@/arkham/cardArt'
import { isMarked, warmCard } from '@/arkham/unwatermark'
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

// The card browser shows every printing as it was printed, rather than the art
// the player's preferences would swap in during a game.
const ignoreVariants = inject<boolean>('cardIgnoreArtVariants', false)

const wantsFlip = ref(flipAll?.value ?? false)

if (flipAll) watch(flipAll, (value) => { wantsFlip.value = value })

const image = computed(() => cardFrontImage(props.card, ignoreVariants))
const backImage = computed(() => cardBackImage(props.card, ignoreVariants))

// Every card can be turned over, generic backs included; only a back whose art
// turns out not to exist (an unimplemented placeholder) loses the flip.
const backMissing = ref(false)
watch(backImage, () => { backMissing.value = false })

const flippable = computed(() => !backMissing.value)
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

/* The CDN watermarks card faces. Each image loads exactly as it always did --
 * which keeps the grid's lazy loading and its layout -- and is then replaced
 * by the same picture without the mark. Nothing unmarked (card backs,
 * homebrew, custom art) is touched. */
const frontClean = ref<string | null>(null)
const backClean = ref<string | null>(null)
let dropFront: (() => void) | null = null
let dropBack: (() => void) | null = null

function reverse(which: 'front' | 'back') {
  const clean = which === 'front' ? frontClean : backClean
  /* Swapping in the reversed image fires load again; the watches below clear
   * this when the card itself changes, so it is the right thing to stop on. */
  if (clean.value) return
  ;(which === 'front' ? dropFront : dropBack)?.()
  const src = which === 'front' ? frontSrc.value : backImage.value
  if (!src || !isMarked(src)) return
  const stop = warmCard(src, (objectUrl) => { clean.value = objectUrl })
  if (which === 'front') dropFront = stop
  else dropBack = stop
}

watch(frontSrc, () => { frontClean.value = null })
watch(backImage, () => { backClean.value = null })
onBeforeUnmount(() => { dropFront?.(); dropBack?.() })

</script>

<template>
  <div class='card-container' :class="[{vertical}]">
    <div class='front' :class="{flipped}">
      <!-- crossorigin is bound BEFORE src on purpose: Vue sets attributes in
           template order, and an <img> starts loading the moment src lands, so
           setting it after would leave the fetch non-CORS and make the reversal
           download the card all over again. -->
      <img
        loading="lazy"
        :crossorigin="isMarked(frontSrc) ? 'anonymous' : undefined"
        :class="['card', 'card-front', { flipped }, attrs.class]"
        :src="frontClean ?? frontSrc"
        @load="reverse('front')"
        @error="onFrontError"
        v-bind="attrs"
      />
      <button v-if="flippable" @click.prevent.stop="wantsFlip = !wantsFlip"><ArrowPathIcon aria-hidden="true" /></button>
    </div>
    <div v-if="flippable" class="back" :class="{flipped}">
      <img
        loading="lazy"
        :crossorigin="isMarked(backImage) ? 'anonymous' : undefined"
        :class="['card', 'card-back', { flipped }, attrs.class]"
        :src="backClean ?? backImage"
        @load="($event) => { updateBackOrientation($event); reverse('back') }"
        @error="backMissing = true"
        v-bind="attrs"
      />
      <button @click.prevent.stop="wantsFlip = !wantsFlip"><ArrowPathIcon aria-hidden="true" /></button>
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
