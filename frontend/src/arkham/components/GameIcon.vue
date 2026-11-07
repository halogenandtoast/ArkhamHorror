<script lang="ts" setup>
import { computed } from 'vue'
import { imgsrc } from '@/arkham/helpers'

/* The game's own token art (the images the pool items and chaos bag use) as a small inline icon, so
 * every map / strip counter reads the same as the physical tokens instead of a text glyph. */
const props = defineProps<{ type: 'clue' | 'doom' | 'resource' | 'health' | 'sanity' | 'damage' | 'horror' }>()

const FILES: Record<string, string> = {
  clue: 'tokens/clue.png', doom: 'tokens/doom.png', resource: 'tokens/resource.png',
  health: 'health.png', sanity: 'horror.png', damage: 'tokens/damage-token.png', horror: 'tokens/horror-token.png',
}
const src = computed(() => imgsrc(FILES[props.type]))
</script>

<template>
  <img class="game-icon" :src="src" alt="" aria-hidden="true" draggable="false" />
</template>

<style scoped>
.game-icon { display: inline-block; width: 1.25em; height: 1.25em; object-fit: contain; vertical-align: -0.28em; filter: drop-shadow(0 1px 1.5px #000a); }
</style>
