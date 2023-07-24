<script lang="ts" setup>

import {computed} from 'vue'
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
  }
}

const keyImage = computed<string>(() => keyToImage(props.name))

</script>

<template>
  <img :src="keyImage" class="key" />
</template>

<style lang="scss" scoped>
.key {
  width: 20px;
}
</style>
