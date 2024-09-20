<script lang="ts" setup>
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'

export interface Props {
  type: string
  amount: number
  tooltip?: string
}
const props = defineProps<Props>()

const emit = defineEmits<{ choose: [] }>()

const image = computed(() => {
  return imgsrc(`${props.type}.png`)
})
</script>

<template>
  <div class="poolItem" :class="`poolItem-${type}`" @click="emit('choose')" v-tooltip="tooltip">
    <img :src="image" />
    <span>{{amount}}</span>
  </div>
</template>

<style scoped lang="scss">

.poolItem {
  pointer-events: inherit;
}

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
}

.poolItem {
  width: 30px;
  display: grid;
  place-items: center;
  color: black;
  font-weight: 900;
  font-size: 1.7em;
  img { filter: drop-shadow(1px 1px 2px rgb(0, 0, 0)); }
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
    color: #F0F0F0;
    text-shadow: 1px 0 0 rgba(0, 0, 0, 0.8), 0 -1px 0 rgba(0, 0, 0, 0.8), 0 1px 0 rgba(0, 0, 0, 0.8), -1px 0 0 rgba(0, 0, 0, 0.8);
    font-size: 0.8em;
    align-items: center;
    justify-content: center;
    aspect-ratio: 1/1;
    border-radius: 50%;
    width: 1.2em;
    height: auto;
  }
}

.resource--can-take, .resource--can-spend {
  pointer-events: auto;
  padding: 0px;
  cursor: pointer;
  background-color: var(--select);
  img { filter: unset; }
}

.health--can-interact, .sanity--can-interact {
  pointer-events: auto;
  > span {
    padding: 0px;
    cursor: pointer;
    border: 2px solid var(--select);
  }
}
</style>
