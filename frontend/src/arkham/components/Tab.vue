<template lang="html">
  <div class='tab' v-show='isActive'>
    <slot></slot>
  </div>
</template>

<script lang="ts">
import { defineComponent, inject, computed } from 'vue';

export default defineComponent({
  props: {
    title: { type: String, required: true },
    index: { type: String, required: true },
    activePlayer: { type: Boolean, required: true },
    playerClass: { type: String, required: true },
    investigatorId: { type: String, required: true },
  },
  setup(props) {
    const selectedTab = inject('selectedTab', props.index)
    const isActive = computed(() => selectedTab == props.index)
    return { isActive }
  }
})
</script>
