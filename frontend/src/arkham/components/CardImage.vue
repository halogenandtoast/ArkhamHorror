<script lang="ts" setup>
import { ref, computed } from 'vue';
import { imgsrc } from '@/arkham/helpers';
import { ArrowPathIcon } from '@heroicons/vue/20/solid'

const props = defineProps<{
  card: CardDef
}>()

const flipped = ref(false)
const vertical = computed(() => {
  if(!flipped.value) return false

  return ["c01121a", "c03241", "c06169a", "c50026a", "c07164a", "c07165a", "c07199a", "c82002a", "c90033a", "c90066a",
  "c03321a", "c04117", "c04118", "c04122", "c04125", "c04126", "c04128", "c04130", "c04133", "c04134", "c04137", "c04209", "c05055", "c05286a", "c05288a", "c10607a", "c53029a", "c53030a", "c53032a", "c53034a", "c53046a", "c82006a"
  ].includes(props.card.cardCode) 
})

const image = computed(() => {
  const {cardType} = props.card 
  if (cardType == 'LocationType' && props.card.doubleSided)
    return imgsrc(`cards/${props.card.art}b.avif`)

  return imgsrc(`cards/${props.card.art}.avif`)
})
const backImage = computed(() => {
  const {cardType, otherSide, doubleSided} = props.card 
  if (otherSide)
    return imgsrc(`cards/${otherSide.replace(/^c/, '')}.avif`)

  if (['ActType', 'AgendaType', 'ScenarioType', 'InvestigatorType'].includes(cardType))
    return imgsrc(`cards/${props.card.art.replace(/a$/, '')}b.avif`)

  if ('LocationType' == cardType) {
    if (props.card.doubleSided)
      return imgsrc(`cards/${props.card.art}.avif`)
    return imgsrc('encounter_back.jpg')
  }

  if (['EnemyType', 'StoryType'].includes(cardType) && props.card.doubleSided)
    return imgsrc(`cards/${props.card.art}b.avif`)

  if (doubleSided)
    return imgsrc(`cards/${props.card.art.replace(/a$/, '')}b.avif`)

  if (['EnemyType', 'StoryType', 'TreacheryType', 'EncounterAssetType', 'EncounterEventType'].includes(cardType))
    return imgsrc('encounter_back.jpg')

  return imgsrc('player_back.jpg')
  
})

</script>

<template>
  <div class='card-container' :class="{vertical}">
    <div class='front' :class="{flipped}">
      <img loading="lazy" class="card card-front" :class="{flipped}" :src="image" />
      <button @click.prevent="flipped = !flipped"><ArrowPathIcon aria-hidden="true" /></button>
    </div>
    <div class="back" :class="{flipped}">
      <img loading="lazy" class="card card-back" :class="{flipped}" :src="backImage" />
      <button @click.prevent="flipped = !flipped"><ArrowPathIcon aria-hidden="true" /></button>
    </div>
  </div>
</template>

<style lang="scss">
.card-container {
  overflow: hidden;
  width: calc(100% - 20px);
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
  }

  &:hover button {
    display: inherit;
  }
}

.front {
  width: 100%;
  backface-visibility: hidden;
  transition: transform 0.3s linear;
  transform-style: preserve-3d;
  &.flipped {
    transform: rotateY(-180deg);
  }
}

.back {
  position:absolute;
  inset: 0;
  transform: rotateY(-180deg);
  transition: transform 0.3s linear;
  transform-style: preserve-3d;
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
