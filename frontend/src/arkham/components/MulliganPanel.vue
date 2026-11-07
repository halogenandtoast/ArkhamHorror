<script lang="ts" setup>
import { computed, onUnmounted } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDbCardStore } from '@/stores/dbCards'
import { cardArt } from '@/arkham/cardImages'
import { toCardContents } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import Card from '@/arkham/components/Card.vue'
import {
  mulliganQuestion,
  mulliganSelected,
  mulliganRunning,
  toggleMulligan,
  startMulligan,
  resetMulligan,
} from '@/arkham/mulligan'

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits<{ (e: 'choose', index: number): void }>()
const { t } = useI18n()
const store = useDbCardStore()

const question = computed(() => mulliganQuestion(props.game, props.playerId))
const picks = computed(() => question.value?.picks ?? [])
const count = computed(() => mulliganSelected.value.length)
const isSelected = (id: string) => mulliganSelected.value.includes(id)

/* Nothing selected: the button keeps the hand, which is the plain "done" choice.
 * Otherwise the whole selection goes out as one confirmed batch. */
const confirm = () => {
  const q = question.value
  if (!q || mulliganRunning.value) return
  if (count.value === 0) {
    emit('choose', q.doneIndex)
    return
  }
  startMulligan()
}

const cardName = (id: string) => {
  const pick = picks.value.find((p) => p.id === id)
  return pick ? store.getDbCard(cardArt(toCardContents(pick.card).cardCode))?.name ?? '' : ''
}

// Selections made for a previous prompt never carry over.
onUnmounted(() => {
  if (!mulliganRunning.value) resetMulligan()
})
</script>

<template>
  <div class="mulligan" :class="{ 'mulligan--running': mulliganRunning }">
    <h2 class="mulligan__title">{{ t('mulligan.title') }}</h2>
    <p class="mulligan__hint">{{ t('mulligan.hint') }}</p>
    <TransitionGroup name="mulligan-card" tag="div" class="mulligan__cards">
      <button
        v-for="pick in picks"
        :key="pick.id"
        type="button"
        class="mulligan__card"
        :class="{ 'mulligan__card--selected': isSelected(pick.id) }"
        :aria-pressed="isSelected(pick.id)"
        :aria-label="cardName(pick.id)"
        :disabled="mulliganRunning"
        @click="toggleMulligan(pick.id)"
      >
        <Card
          :card="pick.card"
          :game="game"
          :playerId="playerId"
          :allowInteractions="false"
          :allowAbilityButtons="false"
        />
        <span class="mulligan__mark" aria-hidden="true">
          <svg viewBox="0 0 24 24"><path d="M5 12l5 5L20 7" /></svg>
        </span>
      </button>
    </TransitionGroup>
    <button type="button" class="mulligan__submit" :disabled="mulliganRunning" @click="confirm">
      {{ mulliganRunning ? t('mulligan.working') : count === 0 ? t('mulligan.keep') : t('mulligan.confirm', { count }, count) }}
    </button>
  </div>
</template>

<style scoped>
.mulligan {
  --card-width: min(130px, 22vw);
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 12px;
  padding: 14px;
  border: 1px solid rgba(214, 205, 174, 0.18);
  border-radius: 10px;
  background: rgba(20, 16, 24, 0.82);
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.3);
  max-width: 100%;
}

.mulligan__title {
  margin: 0;
  font-size: 1.1rem;
}

.mulligan__hint {
  margin: 0;
  color: #8b95a8;
  font-size: 0.8rem;
}

.mulligan__cards {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 10px;
}

.mulligan .mulligan__card {
  position: relative;
  padding: 0;
  border: 2px solid transparent;
  border-radius: 8px;
  background: none;
  background-image: none;
  cursor: pointer;
  /* transform/opacity only: no layout work on every toggle */
  transition: transform 0.18s ease, opacity 0.18s ease, filter 0.18s ease, border-color 0.18s ease;
  will-change: transform;
}

.mulligan .mulligan__card:hover:not(:disabled) {
  transform: translateY(-4px);
}

.mulligan .mulligan__card--selected {
  transform: translateY(-8px) scale(0.96);
  border-color: var(--highlight, #2dd4bf);
  opacity: 0.55;
  filter: grayscale(0.5);
}

.mulligan__card:disabled {
  cursor: default;
}

.mulligan__mark {
  position: absolute;
  top: 6px;
  right: 6px;
  width: 26px;
  height: 26px;
  border-radius: 50%;
  background: var(--highlight, #2dd4bf);
  display: flex;
  align-items: center;
  justify-content: center;
  opacity: 0;
  transform: scale(0.4);
  transition: opacity 0.18s ease, transform 0.18s ease;
  pointer-events: none;
}

.mulligan__card--selected .mulligan__mark {
  opacity: 1;
  transform: scale(1);
}

.mulligan__mark svg {
  width: 70%;
  height: 70%;
  fill: none;
  stroke: #0b0f14;
  stroke-width: 3;
  stroke-linecap: round;
  stroke-linejoin: round;
}

/* Cards leave by fading and shrinking while the rest slide over, so replacing a
   card never makes the row jump. */
.mulligan-card-leave-active {
  position: absolute;
  transition: opacity 0.25s ease, transform 0.25s ease;
}
.mulligan-card-leave-to {
  opacity: 0;
  transform: translateY(-14px) scale(0.8);
}
.mulligan-card-move {
  transition: transform 0.25s ease;
}

.mulligan .mulligan__submit {
  width: 100%;
  padding: 10px 18px;
  border: 0;
  border-radius: 8px;
  background-color: var(--button-2);
  background-image: none;
  color: #eee;
  font-family: inherit;
  font-size: 0.85rem;
  font-weight: bold;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  cursor: pointer;
  transition: background-color 0.2s ease-in;
}

.mulligan .mulligan__submit:hover:not(:disabled) {
  background-color: var(--button-2-highlight);
}

.mulligan .mulligan__submit:disabled {
  background-color: var(--background-dark);
  opacity: 0.6;
  cursor: default;
}

@media (prefers-reduced-motion: reduce) {
  .mulligan .mulligan__card,
  .mulligan__mark,
  .mulligan-card-leave-active,
  .mulligan-card-move {
    transition: none;
  }
}
</style>
