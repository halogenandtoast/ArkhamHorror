<template lang="html">
  <div class='tab' v-show='isActive'>
    <slot></slot>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';

export default defineComponent({
  props: {
    title: { type: String, required: true },
    index: { type: String, required: true },
    selectedTab: { type: String, required: true },
    activePlayer: { type: Boolean, required: true },
    playerClass: { type: String, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const isActive = computed(() => props.selectedTab == props.index)
    return { isActive }
  }
})
</script>
