<script lang="ts" setup>

import {computed, ref} from 'vue'
import {imgsrc} from '@/arkham/helpers'
import { type ArkhamKey } from '@/arkham/types/Key'

const props = defineProps<{
  name: ArkhamKey
}>()

const keyToImage = (k: ArkhamKey): string => {
  switch (k) {
    case "SkullKey": return imgsrc("ct_skull.png")
    case "CultistKey": return imgsrc("ct_cultist.png")
    case "TabletKey": return imgsrc("ct_tablet.png")
    case "ElderThingKey": return imgsrc("ct_elderthing.png")
    case "BlueKey": return imgsrc("keys/blue-key.jpg")
    case "GreenKey": return imgsrc("keys/green-key.jpg")
    case "RedKey": return imgsrc("keys/red-key.jpg")
    case "YellowKey": return imgsrc("keys/yellow-key.jpg")
    case "PurpleKey": return imgsrc("keys/purple-key.jpg")
    case "BlackKey": return imgsrc("keys/black-key.jpg")
    case "WhiteKey": return imgsrc("keys/white-key.jpg")
    case "UnrevealedKey": return imgsrc("keys/key-back.jpg")
  }
}

const keyImage = computed<string>(() => keyToImage(props.name))
const dragging = ref(false)

function startDrag(event: DragEvent) {
  dragging.value = true
  if (event.dataTransfer) {
    event.dataTransfer.effectAllowed = 'move'
    event.dataTransfer.setData('text/plain', JSON.stringify({ "tag": "KeyTarget", "contents": props.name }))
  }
}

</script>

<template>
  <img :src="keyImage" class="key" @dragstart="startDrag($event)" />
</template>

<style lang="scss" scoped>
.key {
  width: 20px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}
</style>
