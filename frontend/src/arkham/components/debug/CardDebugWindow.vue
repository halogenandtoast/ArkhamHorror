<script lang="ts" setup>
/* The debug window for cards that had none: investigators, events, skills,
 * treacheries and stories. Same frame as the enemy window -- card on the left,
 * a column of sections on the right, close along the bottom -- so every card's
 * debug window looks like the same tool. The default slot takes extra sections
 * for whatever a card kind grows later. */
import Draggable from '@/components/Draggable.vue'
import { useEscape } from '@/composable/escape'
import type { Game } from '@/arkham/types/Game'
import type { ChaosToken } from '@/arkham/types/ChaosToken'
import type { SealableTarget } from '@/arkham/debugSeals'
import SealedTokensSection from '@/arkham/components/debug/SealedTokensSection.vue'

defineProps<{
  game: Game
  title: string
  image: string
  sealedTokens: ChaosToken[]
  target: SealableTarget
}>()

const emit = defineEmits<{ close: [] }>()

useEscape(() => emit('close'))
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ title }}</h2></template>
    <div class="debug-modal debug-window">
      <div class="card-debug">
        <img :src="image" class="card-debug__image card-no-overlay" />
        <div class="card-debug__panel">
          <SealedTokensSection :game="game" :tokens="sealedTokens" :target="target" />
          <slot />
        </div>
      </div>
      <button class="debug-close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
.card-debug {
  display: flex;
  flex-flow: row wrap;
  gap: 12px;
  align-items: flex-start;
}

.card-debug__image {
  width: var(--card-width, 120px);
  max-width: 40vw;
  height: auto;
  border-radius: 6px;
}

.card-debug__panel {
  display: flex;
  flex-direction: column;
  gap: 12px;
  flex: 1;
  min-width: 240px;
  max-width: 340px;
}
</style>
