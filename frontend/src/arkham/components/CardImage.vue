<script lang="ts" setup>
import { useAttrs, ref, computed, watch } from 'vue'
import { cardImg, imgsrc } from '@/arkham/helpers'
import { CardDef } from '@/arkham/types/CardDef'
import { ArrowPathIcon } from '@heroicons/vue/20/solid'


defineOptions({
  inheritAttrs: false
})

const attrs = useAttrs()

const props = defineProps<{ card: CardDef }>()

const flipped = ref(false)

// A double-sided card whose OWN art is the 'b' side (e.g. an enemy that is the
// back of an agenda: art "…016b", otherSide "…016"). Its front is the other
// side, so default to showing that and put its own art on the back.
const backPrimary = computed(() => {
  const {otherSide, doubleSided, art} = props.card
  return !!(doubleSided && otherSide && /b$/.test(art)
    && otherSide.replace(/^c/, '') === art.replace(/b$/, ''))
})

const image = computed(() => {
  const {cardType} = props.card
  if (cardType == 'LocationType' && props.card.doubleSided)
    return cardImg(`${props.card.art}b`)

  if (backPrimary.value)
    return cardImg(props.card.otherSide!.replace(/^c/, ''))

  return cardImg(props.card.art)
})
const backImage = computed(() => {
  const {cardType, otherSide, doubleSided} = props.card
  if (backPrimary.value)
    return cardImg(props.card.art)

  if (otherSide)
    return cardImg(otherSide.replace(/^c/, ''))

  if (['ActType', 'AgendaType', 'ScenarioType', 'InvestigatorType'].includes(cardType))
    return cardImg(`${props.card.art.replace(/a$/, '')}b`)

  if ('LocationType' == cardType) {
    if (props.card.doubleSided)
      return cardImg(props.card.art)
    return imgsrc('backs/back_encounter.jpg')
  }

  if (['EnemyType', 'StoryType'].includes(cardType) && props.card.doubleSided)
    return cardImg(`${props.card.art}b`)

  if (doubleSided)
    return cardImg(`${props.card.art.replace(/a$/, '')}b`)

  if (['EnemyType', 'StoryType', 'TreacheryType', 'EncounterAssetType', 'EncounterEventType'].includes(cardType)) {
    if (props.card.meta?.customBack)
      return imgsrc(`backs/${props.card.meta.customBack}`)
    return imgsrc('backs/back_encounter.jpg')
  }

  // Player-type cards (e.g. earned Artifact assets) may also define a custom back.
  if (props.card.meta?.customBack)
    return imgsrc(`backs/${props.card.meta.customBack}`)

  return imgsrc('backs/back_player.jpg')

})

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
        :src="image"
        v-bind="attrs"
      />
      <button @click.prevent="flipped = !flipped"><ArrowPathIcon aria-hidden="true" /></button>
    </div>
    <div class="back" :class="{flipped}">
      <img
        loading="lazy"
        :class="['card', 'card-back', { flipped }, attrs.class]"
        :src="backImage"
        @load="updateBackOrientation"
        v-bind="attrs"
      />
      <button @click.prevent="flipped = !flipped"><ArrowPathIcon aria-hidden="true" /></button>
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
