<script lang="ts" setup>
import { computed, ref } from 'vue'
import { ArrowPathIcon } from '@heroicons/vue/20/solid'
import { useI18n } from 'vue-i18n'
import { useCardFlip } from '@/arkham/composables/useCardFlip'

const props = defineProps<{
  src: string
  back?: string
  passed?: boolean
  current?: boolean
}>()

const { t } = useI18n()

// Only a resolved card has a side to turn to. One still in the deck has an
// unrevealed back, and a history feature must not become a spoiler feature.
const canFlip = computed(() => Boolean(props.passed && props.back))

// Resolved cards open on the side they were resolved on: side A is the face you
// already looked at for a whole phase, side B is the one that flashed past.
const showingBack = ref(true)

const face = computed(() => (canFlip.value && showingBack.value ? props.back! : props.src))
const { displayedImage, flipping } = useCardFlip(face)
</script>

<template>
  <div class="slot">
    <img
      :src="displayedImage"
      class="card slot__card"
      :class="{
        'slot__card--passed': passed,
        'slot__card--current': current,
        'card--flipping': flipping,
      }"
    />
    <button
      type="button"
      class="slot__flip"
      :class="{ 'slot__flip--hidden': !canFlip }"
      :tabindex="canFlip ? undefined : -1"
      :aria-hidden="canFlip ? undefined : 'true'"
      :aria-label="showingBack ? t('card.front') : t('card.resolvedSide')"
      @click="showingBack = !showingBack"
    ><ArrowPathIcon /></button>
  </div>
</template>

<style scoped>
.slot {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 4px;
}

.slot__card {
  width: calc(var(--card-width, 100px) * 1.1);
  border-radius: 6px;
  border: 1px solid rgba(255, 255, 255, 0.38);
  box-sizing: border-box;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.6);
  cursor: zoom-in;
  transition: opacity 0.15s ease, border-color 0.15s ease;
}

.slot__card--passed {
  opacity: 0.4;
  filter: grayscale(0.4);
}

.slot__card--current {
  border-color: transparent;
  outline: 2px solid rgba(255, 255, 255, 0.85);
  outline-offset: 2px;
  box-shadow: 0 0 12px rgba(255, 255, 255, 0.35), 0 2px 8px rgba(0, 0, 0, 0.6);
}

/* Kept in the layout even when a card cannot flip, so every card in the row
   still lines up along the same top edge. */
.slot__flip {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 15px;
  height: 15px;
  padding: 0;
  border-radius: 50%;
  border: 1px solid var(--box-border);
  background: rgba(0, 0, 0, 0.45);
  color: rgba(255, 255, 255, 0.55);
  cursor: pointer;
  transition: background 0.15s ease, color 0.15s ease, border-color 0.15s ease;
}

.slot__flip svg {
  width: 9px;
  height: 9px;
}

/* Neutral, not --select: magenta means the game is waiting on a choice, and
   turning a resolved card over is optional chrome. */
.slot__flip:hover {
  background: rgba(255, 255, 255, 0.14);
  border-color: rgba(255, 255, 255, 0.45);
  color: #fff;
}

.slot__flip:focus-visible {
  outline: 1px solid rgba(255, 255, 255, 0.7);
  outline-offset: 2px;
  color: #fff;
}

.slot__flip--hidden {
  visibility: hidden;
  pointer-events: none;
}
</style>
