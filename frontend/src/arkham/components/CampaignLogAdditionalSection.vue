<script lang="ts" setup>
import FormattedEntry from '@/arkham/components/FormattedEntry.vue'

defineProps<{ title: string; bodyKey: string }>()
</script>

<template>
  <div class="log-section additional-log-section">
    <h3 class="section-title">{{ title }}</h3>
    <div class="additional-log-body">
      <FormattedEntry :entry="{ tag: 'I18nEntry', key: bodyKey, variables: {} }" />
    </div>
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

.additional-log-body {
  color: var(--title);
}

.additional-log-body :deep(img) {
  display: block;
  width: 100%;
  max-width: 800px;
  height: auto;
  border-radius: 6px;
}
</style>
