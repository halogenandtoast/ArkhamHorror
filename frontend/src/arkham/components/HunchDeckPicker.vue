<script lang="ts" setup>
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { formatContent, imgsrc } from '@/arkham/helpers'
import { toCardContents, type Card as ArkhamCard } from '@/arkham/types/Card'
import type { Game } from '@/arkham/types/Game'
import Card from '@/arkham/components/Card.vue'

export interface Props {
  game: Game
  playerId: string
  cards: ArkhamCard[]
  remaining: number
}

const props = defineProps<Props>()
defineEmits<{ choose: [value: number] }>()
const { t } = useI18n()

const investigator = computed(() =>
  Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
)

// The hunch deck is built up by the very question we are answering, so its
// current size is the honest "how far along am I" counter -- Unsolved Case is
// already in it before the first pick.
const hunchDeck = computed(
  () => investigator.value?.decks.find(([key]) => key === 'HunchDeck')?.[1] ?? []
)

// Sorted, never in deck order: the deck is reshuffled after every pick and its
// top card is what Joe reveals each investigation phase.
const inHunchDeck = computed(() =>
  [...hunchDeck.value].sort((a, b) =>
    toCardContents(a).cardCode.localeCompare(toCardContents(b).cardCode)
  )
)

const total = computed(() => hunchDeck.value.length + props.remaining)
const slots = computed<(ArkhamCard | null)[]>(() =>
  Array.from({ length: total.value }, (_, i) => inHunchDeck.value[i] ?? null)
)

const emptySlot = { backgroundImage: `url(${imgsrc('backs/back_player.jpg')})` }
const candidatesLabel = computed(() => formatContent(t('hunchDeck.candidates')))
</script>

<template>
  <div class="hunch-picker">
    <section class="hunch-group">
      <h2>
        <span>{{ t('hunchDeck.deckSoFar') }}</span>
        <span class="hunch-group__note">
          {{ remaining === 0 ? t('hunchDeck.complete') : t('hunchDeck.remaining', { count: remaining }, remaining) }}
        </span>
      </h2>

      <ol class="hunch-slots">
        <li v-for="(card, i) in slots" :key="i" class="hunch-slot">
          <Card
            v-if="card"
            :card="card"
            :game="game"
            :playerId="playerId"
            :allowInteractions="false"
            :allowAbilityButtons="false"
          />
          <span v-else class="hunch-slot__empty" :style="emptySlot"></span>
        </li>
      </ol>
    </section>

    <section class="hunch-group">
      <h2><span v-html="candidatesLabel"></span></h2>

      <div class="hunch-candidates">
        <div v-for="card in cards" :key="toCardContents(card).id" class="hunch-candidate">
          <Card :card="card" :game="game" :playerId="playerId" @choose="$emit('choose', $event)" />
        </div>
      </div>
    </section>
  </div>
</template>

<style scoped>
.hunch-picker {
  display: flex;
  flex-direction: column;
  gap: 10px;
  width: 100%;
  box-sizing: border-box;
}

/* Matches the modal's other card groups so this reads as part of the app. */
.hunch-group {
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 12px;
  border: 1px solid rgba(214, 205, 174, 0.18);
  border-radius: 10px;
  background: rgba(18, 14, 11, 0.7);
  box-shadow:
    inset 0 0 0 1px rgba(0, 0, 0, 0.35),
    0 6px 18px rgba(0, 0, 0, 0.24);
}

.hunch-group h2 {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 12px;
  margin: -2px -2px 2px;
  padding-bottom: 7px;
  border-bottom: 1px solid rgba(214, 205, 174, 0.16);
  color: var(--title);
  font-family: "Teutonic", serif;
  font-size: 1.05rem;
  font-weight: 400;
  letter-spacing: 0.04em;
  line-height: 1;
  text-transform: uppercase;
  text-shadow: 0 1px 2px rgba(0, 0, 0, 0.7);
}

.hunch-group__note {
  flex: 0 0 auto;
  color: var(--seeker);
  font-family: "Noto Sans", sans-serif;
  font-size: 0.8rem;
  letter-spacing: normal;
  text-transform: none;
}

.hunch-slots {
  --card-width: min(52px, 9vw);
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.hunch-slot {
  display: flex;
}

.hunch-slot__empty {
  display: block;
  width: var(--card-width);
  margin: 2px;
  aspect-ratio: var(--card-aspect);
  border-radius: 6px;
  background-position: center;
  background-size: cover;
  opacity: 0.22;
}

.hunch-candidates {
  --card-width: min(78px, 15vw);
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  max-width: min(84vw, 700px);
}

.hunch-candidate {
  display: flex;
  transition: transform 0.15s ease;
}

.hunch-candidate:hover {
  transform: translateY(-4px);
}

@media (max-width: 700px) {
  .hunch-slots {
    --card-width: min(44px, 12vw);
  }

  .hunch-candidates {
    --card-width: min(74px, 20vw);
    max-width: 100%;
  }
}

@media (prefers-reduced-motion: reduce) {
  .hunch-candidate {
    transition: none;
  }

  .hunch-candidate:hover {
    transform: none;
  }
}
</style>
