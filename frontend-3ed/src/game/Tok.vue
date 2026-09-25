<script setup lang="ts">
import { computed } from 'vue'
import { TOKEN } from '@/game/util'

const props = withDefaults(
  defineProps<{ name: string; count?: number | null; title?: string; size?: number; always?: boolean }>(),
  { count: null, title: undefined, size: 28, always: false },
)
const label = computed(() => props.title ?? props.name)
const showCount = computed(() => props.count !== undefined && props.count !== null && (props.always || props.count !== 1))
</script>

<template>
  <span class="tok" :title="label" :style="{ width: `${size}px`, height: `${size}px` }"
    ><img :src="TOKEN(name)" :alt="label" /><b v-if="showCount">{{ count }}</b></span
  >
</template>
