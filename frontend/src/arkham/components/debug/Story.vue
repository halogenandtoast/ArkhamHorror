<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue'
import { computed } from 'vue'
import type { Story } from '@/arkham/types/Story'
import type { Game } from '@/arkham/types/Game'
import { readTokenBag } from '@/arkham/types/TokenBag'
import TokenBag from './TokenBag.vue'

const props = defineProps<{ story: Story, game: Game }>()
const emit = defineEmits<{ close: [] }>()
const bag = computed(() => readTokenBag(props.story.meta))
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.story.title') }}</h2></template>
    <div class="bag-container">
      <TokenBag v-if="bag" :game-id="game.id" :story-id="story.id" :bag="bag" />
      <p v-else>{{ $t('debug.story.empty') }}</p>
      <button class="button close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
h2 { font-family: teutonic, sans-serif; font-weight: normal; letter-spacing: .04em; }
.bag-container { display: flex; flex-direction: column; gap: 16px; width: min(400px, 85vw); max-height: 75dvh; overflow-y: auto; padding: 16px; box-sizing: border-box; background: var(--box-background); color: var(--text); }
button.close { border: 1px solid var(--box-border); border-radius: 4px; padding: 10px; background: var(--background-dark); color: var(--text); width: 100%; cursor: pointer; }
button.close:hover { background: var(--background-mid); }
button.close:focus-visible { outline: 2px solid var(--spooky-green); outline-offset: 2px; }
</style>
