<script lang="ts" setup>
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'

export interface Props {
  type: string
  amount: number
}
const props = defineProps<Props>()

const image = computed(() => {
  return imgsrc(`${props.type}.png`)
})
</script>

<template>
  <div class="poolItem" :class="`poolItem-${type}`" @click="$emit('choose')">
    <img :src="image" />
    <span>{{amount}}</span>
  </div>
</template>

<style scoped lang="scss">

.poolItem.poolItem-resource {
  img {
    width: 30px;
  }
  padding: 2px;
  clip-path: polygon(50% 0%, 100% 25%, 100% 75%, 50% 100%, 0% 75%, 0% 25%);
}

.poolItem.poolItem-clue {
  img {
    width: 30px;
  }
  height: 35px;
  width: 35px;
  border-radius: 35px;
}

.poolItem {
  width: 30px;
  display: grid;
  place-items: center;
  isolation: isolate;
  color: black;
  font-weight: 900;
  font-size: 1.7em;
  & > * {
    grid-column: 1 / -1;
    grid-row: 1 / -1;
  }

  img {
    width: 100%;
  }

  span {
    font-family: "Arkham";
    display: flex;
    position: relative;
    background: rgba(255,255,255,0.5);
    border-radius: 20px;
    font-size: 0.8em;
    width: 1.05em;
    height: 1.05em;
    align-items: center;
    justify-content: center;
  }
}

.resource--can-take, .resource--can-spend {
  padding: 0px;
  cursor: pointer;
  background-color: $select;
}

.health--can-interact, .sanity--can-interact {
  > span {
    padding: 0px;
    cursor: pointer;
    border: 2px solid $select;
  }
}
</style>
