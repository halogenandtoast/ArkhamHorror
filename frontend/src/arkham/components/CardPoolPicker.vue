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
  /** Cards that can still be added to the pool. */
  cards: ArkhamCard[]
  /** Cards already in the pool. */
  chosen: ArkhamCard[]
  remaining: number
  titleKey: string
  candidatesKey: string
  accent: string
}

const props = defineProps<Props>()
defineEmits<{ choose: [value: number] }>()
const { t } = useI18n()

// Sorted, never in pool order: these decks are shuffled as they are built, so
// their order says nothing and a stable order keeps slots from jumping around.
const inPool = computed(() =>
  [...props.chosen].sort((a, b) =>
    toCardContents(a).cardCode.localeCompare(toCardContents(b).cardCode)
  )
)

const total = computed(() => props.chosen.length + props.remaining)
const slots = computed<(ArkhamCard | null)[]>(() =>
  Array.from({ length: total.value }, (_, i) => inPool.value[i] ?? null)
)

const emptySlot = { backgroundImage: `url(${imgsrc('backs/back_player.jpg')})` }
const title = computed(() => formatContent(t(props.titleKey)))
const candidatesLabel = computed(() => formatContent(t(props.candidatesKey)))
</script>

<template>
  <div class="card-pool-picker" :class="`pool--${accent}`">
    <section class="pool-group">
      <h2>
        <span v-html="title"></span>
        <span class="pool-group__note">
          {{ remaining === 0 ? t('cardPool.complete') : t('cardPool.remaining', { count: remaining }, remaining) }}
        </span>
      </h2>

      <ol class="pool-slots">
        <li v-for="(card, i) in slots" :key="i" class="pool-slot">
          <Card
            v-if="card"
            :card="card"
            :game="game"
            :playerId="playerId"
            :allowInteractions="false"
            :allowAbilityButtons="false"
          />
          <span v-else class="pool-slot__empty" :style="emptySlot"></span>
        </li>
      </ol>
    </section>

    <section class="pool-group">
      <h2><span v-html="candidatesLabel"></span></h2>

      <div class="pool-candidates">
        <div v-for="card in cards" :key="toCardContents(card).id" class="pool-candidate">
          <Card :card="card" :game="game" :playerId="playerId" @choose="$emit('choose', $event)" />
        </div>
      </div>
    </section>
  </div>
</template>

<style scoped>
.card-pool-picker {
  display: flex;
  flex-direction: column;
  gap: 10px;
  width: 100%;
  box-sizing: border-box;
}

/* Matches the modal's other card groups so this reads as part of the app. */
.pool-group {
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

.pool-group h2 {
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

.pool-group__note {
  flex: 0 0 auto;
  color: var(--pool-accent, var(--seeker));
  font-family: "Noto Sans", sans-serif;
  font-size: 0.8rem;
  letter-spacing: normal;
  text-transform: none;
}

.pool-slots {
  --card-width: min(52px, 9vw);
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.pool-slot {
  display: flex;
}

.pool-slot__empty {
  display: block;
  width: var(--card-width);
  margin: 2px;
  aspect-ratio: var(--card-aspect);
  border-radius: 6px;
  background-position: center;
  background-size: cover;
  opacity: 0.22;
}

.pool-candidates {
  --card-width: min(78px, 15vw);
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  max-width: min(84vw, 700px);
}

.pool-candidate {
  display: flex;
  transition: transform 0.15s ease;
}

.pool-candidate:hover {
  transform: translateY(-4px);
}

@media (max-width: 700px) {
  .pool-slots {
    --card-width: min(44px, 12vw);
  }

  .pool-candidates {
    --card-width: min(74px, 20vw);
    max-width: 100%;
  }
}

@media (prefers-reduced-motion: reduce) {
  .pool-candidate {
    transition: none;
  }

  .pool-candidate:hover {
    transform: none;
  }
}
</style>
