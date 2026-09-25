<script setup lang="ts">
import Tok from '@/game/Tok.vue'
import { MYTHOS } from '@/game/util'

defineProps<{ token: string; size?: number }>()
const title = (t: string) => t.replace(/Token$/, '').replace(/([a-z])([A-Z])/g, '$1 $2')
</script>

<template>
  <Tok :name="MYTHOS[token] ?? 'mythos-blank'" :title="title(token)" :size="size ?? 28" />
</template>
