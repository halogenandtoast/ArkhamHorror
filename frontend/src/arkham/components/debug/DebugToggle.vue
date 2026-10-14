<script lang="ts" setup>
/* The Debug button of a panel's hidden debug controls (see `useShiftHoverDebug`);
 * the parent places it and handles the click. */
defineProps<{ active: boolean }>()
</script>

<template>
  <button type="button" class="debug-toggle" :class="{ active }" :aria-pressed="active">
    {{ $t('debug.campaign.debugToggle') }}
  </button>
</template>

<style scoped>
.debug-toggle {
  appearance: none;
  border: 1px solid #9d3030;
  border-radius: 3px;
  background: #6d1f1f;
  color: white;
  padding: 3px 8px;
  font-size: 0.8em;
  line-height: 1.2;
  cursor: pointer;
  opacity: 0.85;
}

.debug-toggle:hover {
  opacity: 1;
}

.debug-toggle.active {
  background: #9d3030;
  border-color: #e06060;
  opacity: 1;
}
</style>
