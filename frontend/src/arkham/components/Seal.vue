<script lang="ts" setup>

import {computed, ref} from 'vue'
import {imgsrc} from '@/arkham/helpers'
import { type Seal } from '@/arkham/types/Seal'

const props = defineProps<{
  seal: Seal
}>()

const sealToImage = (k: Seal): string => {
  const revealed = k.sealActive ? "active" : "dormant"
  switch (k.sealKind) {
    case "SealA": return imgsrc(`extra/edge-of-the-earth/seals/seal-a-${revealed}.png`)
    case "SealB": return imgsrc(`extra/edge-of-the-earth/seals/seal-b-${revealed}.png`)
    case "SealC": return imgsrc(`extra/edge-of-the-earth/seals/seal-c-${revealed}.png`)
    case "SealD": return imgsrc(`extra/edge-of-the-earth/seals/seal-d-${revealed}.png`)
    case "SealE": return imgsrc(`extra/edge-of-the-earth/seals/seal-e-${revealed}.png`)
  }
}

const sealImage = computed<string>(() => sealToImage(props.seal))
</script>

<template>
  <img :src="sealImage" class="seal" />
</template>

<style scoped>
.seal {
  width: 25px;
  -webkit-filter: drop-shadow(2px 2px 2px #111);
  filter: drop-shadow(2px 2px 2px #111);
}
</style>
