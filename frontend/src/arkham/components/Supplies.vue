<script lang="ts" setup>
import { computed } from 'vue';
import { type Investigator } from '@/arkham/types/Investigator'

const props = defineProps<{
  player: Investigator
}>()

const supplies = computed(() =>
  props.player.supplies.reduce((acc, supply) => acc.set(supply, (acc.get(supply) || 0) + 1), new Map<string, number>())
)
</script>

<template>
  <div v-if="supplies.size > 0" class="supplies box">
    <slot name="heading" />
    <ul>
      <li v-for="[supply, count] in supplies.entries()" :key="supply">{{supply}} ({{count}})</li>
    </ul>
  </div>
</template>

<style scoped lang="scss">
.supplies {
  padding: 10px 20px;
  flex: 1;
}

ul {
  list-style-position: inside;
}

</style>
