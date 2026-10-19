<script lang="ts" setup>
// Rendered by Toasts.vue inside the main app, so i18n and the app's global
// components are both available here. It still takes pre-translated strings as
// props, and imports FontAwesomeIcon explicitly, to stay a dumb presenter.
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

defineProps<{ title: string; name: string; text: string }>()
</script>

<template>
  <div class="achievement-unlock">
    <FontAwesomeIcon :icon="['fas', 'trophy']" class="trophy" aria-hidden="true" />
    <div class="body">
      <span class="unlock-title">{{ title }}</span>
      <span class="name">{{ name }}</span>
      <span class="text">{{ text }}</span>
    </div>
  </div>
</template>

<style>
/* Global: reskins the toast wrapper Toasts.vue draws around this component,
   selected by the `className` passed to toast.custom(). Setting the accent is
   enough for the progress bar -- a custom toast draws no variant strip. */
.toasts .toast.achievement-toast {
  --toast-accent: #b3922f;
  background: linear-gradient(135deg, #2c2413 0%, #1d1a10 100%);
  border: 1px solid #b3922f;
  box-shadow: 0 6px 24px rgba(0, 0, 0, 0.55), 0 0 12px rgba(179, 146, 47, 0.35);
  color: #f0e6c8;
}
</style>

<style scoped>
.achievement-unlock {
  display: flex;
  align-items: flex-start;
  gap: 12px;
  /* A flex item of the toast wrapper: both are needed for the text to wrap. */
  flex: 1;
  min-width: 0;
}

.trophy {
  color: #d9b845;
  font-size: 1.6em;
  margin-top: 2px;
  flex-shrink: 0;
  filter: drop-shadow(0 0 6px rgba(217, 184, 69, 0.5));
}

.body {
  display: flex;
  flex-direction: column;
  gap: 2px;
  min-width: 0;
}

.unlock-title {
  font-size: 0.7rem;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: #d9b845;
}

.name {
  font-weight: 700;
  font-size: 1rem;
  color: #f7efd8;
}

.text {
  font-size: 0.82rem;
  line-height: 1.4;
  color: rgba(240, 230, 200, 0.75);
}
</style>
