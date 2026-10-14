<script lang="ts" setup>
/* The bag's faces in bag order with a count on each; a click seals one token of
 * that face. Stays open after a click, and the counts come from game state, so
 * they fall as each seal lands. */
import { computed } from 'vue'
import Draggable from '@/components/Draggable.vue'
import { useDebug } from '@/arkham/debug'
import { useEscape } from '@/composable/escape'
import type { Game } from '@/arkham/types/Game'
import { chaosTokenImage } from '@/arkham/types/ChaosToken'
import { groupByFace, sealMessage, type FaceGroup, type SealableTarget } from '@/arkham/debugSeals'

const props = defineProps<{
  game: Game
  target: SealableTarget
}>()

const emit = defineEmits<{ close: [] }>()

useEscape(() => emit('close'))

const debug = useDebug()

const groups = computed(() => groupByFace(props.game.scenario?.chaosBag.chaosTokens ?? []))

const seal = (group: FaceGroup) => {
  const token = group.tokens[0]
  if (token) debug.send(props.game.id, sealMessage(token.id, props.target))
}
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.seals.pickerTitle') }}</h2></template>
    <div class="debug-modal debug-window seal-picker">
      <p class="seal-picker__hint">{{ $t('debug.seals.pickerHint') }}</p>
      <p v-if="groups.length === 0" class="seal-picker__empty">{{ $t('debug.seals.emptyBag') }}</p>
      <div v-else class="seal-picker__tokens">
        <button
          v-for="group in groups"
          :key="group.face"
          type="button"
          class="seal-picker__token"
          @click="seal(group)"
        >
          <img :src="chaosTokenImage(group.face)" :alt="group.face" />
          <span class="count-pill">{{ group.tokens.length }}</span>
        </button>
      </div>
      <button class="debug-close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
    </div>
  </Draggable>
</template>

<style scoped>
.seal-picker {
  max-width: 420px;
  color: var(--text);
  font-size: 0.85rem;
}

.seal-picker__hint,
.seal-picker__empty {
  margin: 0;
  color: rgba(255, 255, 255, 0.7);
  line-height: 1.4;
}

.seal-picker__tokens {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 8px;
}

.seal-picker__token {
  position: relative;
  display: grid;
  place-items: center;
  padding: 4px;
  border: 1px solid transparent;
  border-radius: 999px;
  background: none;
  cursor: pointer;
  transition: border-color 0.12s ease, background-color 0.12s ease;
}

.seal-picker__token:hover {
  border-color: var(--select);
  background: rgba(255, 255, 255, 0.08);
}

.seal-picker__token img {
  width: 44px;
  height: auto;
  display: block;
}

.seal-picker__token .count-pill {
  position: absolute;
  right: -4px;
  bottom: -2px;
  font-size: 0.7rem;
}
</style>
