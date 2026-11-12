<script lang="ts" setup>
/* A toggle button inside a panel's debug controls -- the parent places it and
 * handles the click. Whether the controls show at all is the game bar's debug
 * mode (`useDebug().active`), so this is for a choice *within* them, such as
 * opening the campaign-log entry editor. */
defineProps<{ active: boolean }>()
</script>

<template>
  <button type="button" class="debug-toggle" :class="{ active }" :aria-pressed="active">
    <slot>{{ $t('debug.campaign.debugToggle') }}</slot>
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
