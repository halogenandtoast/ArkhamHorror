<script setup lang="ts">
import MiniChart from './MiniChart.vue'

withDefaults(defineProps<{
  title: string
  value: string | number
  sublabel?: string
  trend?: number[]
  accent?: string
  showDelta?: boolean
}>(), {
  trend: () => [],
  accent: '#7dd3fc',
  showDelta: true,
})
</script>

<template>
  <div class="stat-card">
    <div class="stat-head">
      <h3>{{ title }}</h3>
      <span v-if="sublabel" class="sub">{{ sublabel }}</span>
    </div>

    <div class="stat-value">{{ value }}</div>

    <MiniChart
      v-if="trend && trend.length"
      :data="trend"
      :stroke="accent"
      :showDelta="showDelta"
    />
  </div>
</template>

<style scoped>
.stat-card {
  background: linear-gradient(180deg, #14161a, #171a1f);
  border: 1px solid #2a2f37;
  border-radius: 16px;
  padding: 16px;
  box-shadow: 0 10px 28px rgba(0,0,0,.32);
  display: flex; flex-direction: column; gap: 8px;
  transition: transform .18s ease, box-shadow .18s ease, border-color .18s ease;
}
.stat-card:hover { transform: translateY(-2px); border-color: rgba(125,211,252,.25) }

.stat-head { display: flex; align-items: baseline; gap: 10px; }
h3 { font-size: .9rem; color: #a8b0bf; margin: 0; }
.sub { font-size: .75rem; color: #7e8899; }

.stat-value {
  font-size: 1.9rem;
  font-weight: 800;
  letter-spacing: .01em;
}
</style>
