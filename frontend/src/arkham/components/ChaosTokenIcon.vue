<script lang="ts" setup>
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'

/* The game's own chaos-token art (the same images the chaos bag uses) for a symbol in a reference
 * card's token list, so the symbols match the physical tokens exactly. */
const props = defineProps<{ token: string }>()

const FILES: Record<string, string> = {
  skull: 'ct-skull', cultist: 'ct-cultist', tablet: 'ct-tablet', elder_thing: 'ct-elderthing',
  auto_fail: 'ct-autofail', elder_sign: 'ct-eldersign',
}
const src = computed(() => imgsrc(`chaos-tokens/${FILES[props.token] ?? 'ct-blank'}.png`))
</script>

<template>
  <img class="chaos-icon" :src="src" :alt="token.replace('_', ' ')" loading="lazy" />
</template>

<style scoped>
.chaos-icon { width: 100%; height: 100%; display: block; object-fit: contain; filter: drop-shadow(0 1px 2px #000a); }
</style>
