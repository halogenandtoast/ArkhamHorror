<script lang="ts" setup>
// Rendered inside vue-toastification's own app instance, which does not share
// the main app's global components — import FontAwesomeIcon locally and pass
// pre-translated strings as props (no i18n context in here).
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
/* Global: restyle the vue-toastification wrapper (toastClassName option). */
.Vue-Toastification__toast.achievement-toast {
  background: linear-gradient(135deg, #2c2413 0%, #1d1a10 100%);
  border: 1px solid #b3922f;
  box-shadow: 0 6px 24px rgba(0, 0, 0, 0.55), 0 0 12px rgba(179, 146, 47, 0.35);
  color: #f0e6c8;
}

.Vue-Toastification__toast.achievement-toast .Vue-Toastification__progress-bar {
  background: #b3922f;
}
</style>

<style scoped>
.achievement-unlock {
  display: flex;
  align-items: flex-start;
  gap: 12px;
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
