<script lang="ts" setup>
/* The Science Expansion shop, offered on the continuation screen between any two
 * scenarios.
 *
 * Every card's cost is printed on its own face ("Researched (3 experience and 1
 * Memories)"), so the panel's whole job is to show the cards big enough to read
 * and to say what the buyer still has to spend. Deliberately not routed through
 * StoryQuestion's generic card-label row: that marks its images `no-overlay`, so
 * the cards cannot be zoomed, which is exactly what reading a shop needs.
 */
import { computed } from 'vue'
import { cardImage } from '@/arkham/cardImages'
import type { Game } from '@/arkham/types/Game'
import { useI18n } from 'vue-i18n'

const props = defineProps<{ game: Game; playerId: string; viewOnly?: boolean }>()
const emit = defineEmits<{ choose: [value: number] }>()
const { t } = useI18n()

const buyer = computed(() =>
  Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
)

// Memories are a per-investigator tally, so they live in that investigator's own
// log rather than the campaign's. The key is the campaign's own enum lifted
// through the shared homebrew wrapper, hence the "darkMatter." prefix.
const memories = computed(() => {
  const counts = buyer.value?.log?.recordedCounts ?? []
  const entry = counts.find(
    ([key]) => key.tag === 'HomebrewCampaignLogKey' && key.contents === 'darkMatter.Memories'
  )
  return entry?.[1] ?? 0
})

const question = computed(() => {
  const q = props.game.question[props.playerId]
  return q?.tag === 'QuestionLabel' ? q.question : q
})

const choices = computed(() => {
  const inner = question.value
  if (inner?.tag !== 'ChooseOne') return []
  return inner.choices.map((choice: any, index: number) => ({ choice, index }))
})

const cards = computed(() =>
  choices.value
    .filter(({ choice }) => choice.tag === 'CardLabel')
    .map(({ choice, index }) => ({ index, cardCode: choice.cardCode as string }))
)

// The one non-card choice is "do not purchase anything"; it ends this
// investigator's turn at the shop.
const declineIndex = computed(() => {
  const entry = choices.value.find(({ choice }) => choice.tag !== 'CardLabel')
  return entry?.index ?? null
})

function pick(index: number) {
  if (props.viewOnly) return
  emit('choose', index)
}
</script>

<template>
  <div class="science-shop" :class="{ 'view-only': viewOnly }">
    <h2 class="science-shop__title">{{ t('darkMatter.scienceExpansion.title') }}</h2>

    <p v-if="viewOnly" class="science-shop__hint">
      {{ t('darkMatter.scienceExpansion.waiting', { investigator: buyer?.name?.title ?? '' }) }}
    </p>
    <p v-else class="science-shop__hint">{{ t('darkMatter.scienceExpansion.instructions') }}</p>

    <div class="science-shop__purse">
      <span class="science-shop__coin">
        {{ t('darkMatter.scienceExpansion.experienceRemaining', { count: buyer?.xp ?? 0 }) }}
      </span>
      <span class="science-shop__coin">
        {{ t('darkMatter.scienceExpansion.memoriesRemaining', { count: memories }) }}
      </span>
    </div>

    <div class="science-shop__cards">
      <button
        v-for="{ index, cardCode } in cards"
        :key="cardCode"
        type="button"
        class="science-shop__card"
        :disabled="viewOnly"
        @click="pick(index)"
      >
        <img class="card" :src="cardImage(cardCode)" :alt="cardCode" />
      </button>
    </div>

    <button
      v-if="declineIndex !== null && !viewOnly"
      type="button"
      class="science-shop__done"
      @click="pick(declineIndex)"
    >
      {{ t('darkMatter.scienceExpansion.donePurchasing') }}
    </button>
  </div>
</template>

<style scoped>
.science-shop {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 12px;
  margin: 5vh auto;
  padding: 20px 24px;
  max-width: min(1100px, 92vw);
  border-radius: 10px;
  background: var(--box-background);
  border: 1px solid rgba(96, 170, 182, 0.45);
}

.science-shop__title {
  margin: 0;
  font-family: teutonic, sans-serif;
  font-weight: normal;
  font-size: 1.6em;
  color: #8fd3de;
}

.science-shop__hint {
  margin: 0;
  color: #cfd8da;
  text-align: center;
}

.science-shop__purse {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  justify-content: center;
}

.science-shop__coin {
  padding: 4px 12px;
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.35);
  border: 1px solid rgba(143, 211, 222, 0.35);
  color: #e6f4f6;
  font-size: 0.9em;
}

.science-shop__cards {
  display: flex;
  flex-wrap: wrap;
  gap: 14px;
  justify-content: center;
}

/* Twice the board's card width, so the printed "Researched" cost and the ability
 * text are legible without reaching for the overlay -- and the images keep the
 * overlay too (no `no-overlay`), so hovering still zooms one to full size. */
.science-shop__card {
  padding: 0;
  border: 0;
  border-radius: 8px;
  background: none;
  cursor: pointer;
  line-height: 0;
  transition: transform 0.12s ease-out;
}

.science-shop__card:hover:not(:disabled) {
  transform: translateY(-4px);
}

.science-shop__card:disabled {
  cursor: default;
}

.science-shop__card .card {
  width: calc(var(--card-width) * 2);
  max-width: 40vw;
  border-radius: 8px;
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.53);
}

.science-shop__done {
  border: 1px solid rgba(143, 211, 222, 0.35);
  border-radius: 8px;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  padding: 8px 20px;
  cursor: pointer;
}

.science-shop__done:hover {
  background: rgba(0, 0, 0, 0.5);
}

.view-only {
  opacity: 0.85;
}
</style>
