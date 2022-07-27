<script lang="ts" setup>
import { computed, inject } from 'vue'

export interface Props {
  type: string
  amount: number
}
const props = defineProps<Props>()
const baseUrl = inject('baseUrl')

const image = computed(() => {
  return `${baseUrl}/img/arkham/${props.type}.png`
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
    margin: 4px 0 0 2px;
  }
  height: 40px;
  width: 34px;
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
  position: relative;
  width: 30px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: black;
  font-weight: 900;
  font-size: 1.7em;

  img {
    width: 100%;
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    margin: auto;
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
