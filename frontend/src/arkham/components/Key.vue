<script lang="ts" setup>

import {computed} from 'vue'
import {imgsrc} from '@/arkham/helpers'

export interface Props {
  name: string
}

const props = defineProps<Props>()

const keyImage = computed(() => {
  switch (props.name) {
    case "SkullKey": return imgsrc("ct_skull.png")
    case "CultistKey": return imgsrc("ct_cultist.png")
    case "TabletKey": return imgsrc("ct_tablet.png")
    case "ElderThingKey": return imgsrc("ct_elderthing.png")
    default: return props.name satisfies never
  }
})

</script>

<template>
  <img :src="keyImage" class="key" />
</template>

<style lang="scss" scoped>
.key {
  width: 20px;
}
</style>
