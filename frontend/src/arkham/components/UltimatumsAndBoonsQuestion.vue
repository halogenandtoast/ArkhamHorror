<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Game } from '@/arkham/types/Game'
import { cardImage, toCardContents } from '@/arkham/types/Card'
import { imgsrc } from '@/arkham/helpers'

const props = defineProps<{ game: Game; playerId: string }>()
const emit = defineEmits<{ choose: [value: number] }>()
const { t } = useI18n()

// The Morrígan question is a QuestionLabel-wrapped ChooseOne of CardIdTarget
// labels, one per focused weakness; choice index i returns focused card i.
const choices = computed(() => {
  const q = props.game.question[props.playerId]
  const inner = q?.tag === 'QuestionLabel' ? q.question : q
  if (inner?.tag !== 'ChooseOne') return []
  return inner.choices.flatMap((choice: any, index: number) => {
    if (choice.tag !== 'TargetLabel' || choice.target?.tag !== 'CardIdTarget') return []
    const cardId = choice.target.contents
    const card = props.game.focusedCards.flat().find((c) => toCardContents(c).id === cardId)
    return card ? [{ index, card }] : []
  })
})
</script>

<template>
  <div class="morrigan-panel">
    <h2 class="morrigan-title">
      <span class="morrigan-icon" aria-hidden="true">✦</span>
      {{ t('ultimatumsAndBoons.entries.BoonOfTheMorrigan.name') }}
    </h2>
    <p class="morrigan-instructions">{{ t('ultimatumsAndBoons.morrigan.instructions') }}</p>
    <div class="weakness-cards">
      <button
        v-for="{ index, card } in choices"
        :key="toCardContents(card).id"
        type="button"
        class="weakness-card"
        @click="emit('choose', index)"
      >
        <img :src="imgsrc(cardImage(card))" :alt="toCardContents(card).cardCode" />
        <span class="return-label">{{ t('ultimatumsAndBoons.morrigan.returnAction') }}</span>
      </button>
    </div>
  </div>
</template>

<style scoped>
.morrigan-panel {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 12px;
  padding: 20px 24px;
  border-radius: 10px;
  background: var(--box-background);
  border: 1px solid rgba(179, 146, 47, 0.45);
}

.morrigan-title {
  font-family: teutonic, sans-serif;
  font-weight: normal;
  font-size: 1.6em;
  color: #d8bc62;
  margin: 0;
  display: flex;
  align-items: baseline;
  gap: 10px;
}

.morrigan-icon {
  color: #b3922f;
}

.morrigan-instructions {
  margin: 0;
  max-width: 60ch;
  text-align: center;
  color: rgba(255, 255, 255, 0.75);
  line-height: 1.5;
}

.weakness-cards {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 18px;
  margin-top: 6px;
}

.weakness-card {
  position: relative;
  display: flex;
  flex-direction: column;
  padding: 0;
  border: 0;
  background: none;
  cursor: pointer;
  border-radius: 12px;
  overflow: hidden;
  transition: transform 0.15s ease;

  img {
    width: clamp(180px, 22vw, 260px);
    border-radius: 12px;
    display: block;
  }

  .return-label {
    position: absolute;
    inset: auto 0 0 0;
    padding: 8px 0;
    background: rgba(0, 0, 0, 0.75);
    color: #d8bc62;
    font-weight: 600;
    opacity: 0;
    transition: opacity 0.15s ease;
  }

  &:hover, &:focus-visible {
    transform: translateY(-4px);
    outline: 2px solid #b3922f;

    .return-label { opacity: 1; }
  }
}
</style>
