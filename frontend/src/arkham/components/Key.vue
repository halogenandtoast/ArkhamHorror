<script lang="ts" setup>

import {inject,computed} from 'vue'

export interface Props {
  name: string
}

const props = defineProps<Props>()
const baseUrl = inject('baseUrl')

const keyImage = computed(() => {
  switch (props.name) {
    case "SkullKey": return `${baseUrl}/img/arkham/ct_skull.png`
    case "CultistKey": return `${baseUrl}/img/arkham/ct_cultist.png`
    case "TabletKey": return `${baseUrl}/img/arkham/ct_tablet.png`
    case "ElderThingKey": return `${baseUrl}/img/arkham/ct_elderthing.png`
    default: return ""
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
