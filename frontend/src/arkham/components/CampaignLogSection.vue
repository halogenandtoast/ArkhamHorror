<script lang="ts" setup>
defineProps<{ title: string; items: string[] }>()
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ title }}</h3>
    <ul class="log-list" v-if="items.length > 0">
      <li v-for="item in items" :key="item">{{ item }}</li>
    </ul>
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

.log-list {
  display: flex;
  flex-direction: column;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;

  li {
    display: flex;
    align-items: baseline;
    gap: 8px;
    margin: 0;
    padding: 7px 10px;
    border-radius: 5px;
    background: rgba(255,255,255,0.04);
    color: var(--title);
    font-size: 0.92rem;
    line-height: 1.4;
    list-style: none;

    &::before {
      content: '–';
      color: rgba(255,255,255,0.25);
      flex-shrink: 0;
    }
  }
}
</style>
