<script setup lang="ts">
import { computed } from 'vue'
import { TOKEN, TOKEN_TIP } from '@/game/util'

const props = withDefaults(
  defineProps<{ name: string; count?: number | null; title?: string; size?: number; always?: boolean }>(),
  { count: null, title: undefined, size: 28, always: false },
)
const label = computed(() => props.title ?? props.name)
// what this token is and does; the shared tooltip picks it up from the attributes
const tip = computed(() => TOKEN_TIP[props.name] ?? null)
const showCount = computed(() => props.count !== undefined && props.count !== null && (props.always || props.count !== 1))
</script>

<template>
  <span
    class="tok"
    :title="tip ? undefined : label"
    :data-tip="tip ? tip[0] : undefined"
    :data-tip-body="tip ? tip[1] : undefined"
    :style="{ width: `${size}px`, height: `${size}px` }"
    ><img :src="TOKEN(name)" :alt="label" /><b v-if="showCount">{{ count }}</b></span
  >
</template>
