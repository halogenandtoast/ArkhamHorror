<script setup lang="ts">
/* A symbol from the game's icon font: <Icon name="clue" /> for the clue glyph.
It takes the colour and size of the text around it unless told otherwise. */
import { computed } from 'vue'
import { ICON } from '@/game/util'

const props = defineProps<{ name: string; size?: number | string; title?: string }>()
const glyph = computed(() => ICON[props.name] ?? '')
const style = computed(() => (props.size ? { fontSize: typeof props.size === 'number' ? `${props.size}px` : props.size } : undefined))
</script>

<template>
  <span
    v-if="glyph"
    class="ico"
    :style="style"
    :title="title"
    :role="title ? 'img' : undefined"
    :aria-label="title"
    :aria-hidden="title ? undefined : 'true'"
    >{{ glyph }}</span
  >
</template>
