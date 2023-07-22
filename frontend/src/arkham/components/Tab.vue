<script lang="ts" setup>
import { computed } from 'vue';

export interface Props {
  title: string
  index: string
  selectedTab: string
  activePlayer: boolean
  playerClass: string
  investigatorId: string
}

const props = defineProps<Props>()

const isActive = computed(() => props.selectedTab == props.index)
</script>

<template>
  <div class='tab' v-show='isActive'>
    <slot></slot>
  </div>
</template>
