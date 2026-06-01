<script lang="ts" setup>
import { computed } from 'vue'
import * as Arkham from '@/arkham/types/CardDef'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import CardImage from '@/arkham/components/CardImage.vue'

const props = withDefaults(defineProps<{ cards: Arkham.CardDef[], attachments?: Record<string, Arkham.CardDef[]>, showCounts?: boolean }>(), {
  attachments: () => ({}),
  showCounts: true,
})

const groupedCards = computed(() => {
  return props.cards.reduce<Array<{ card: Arkham.CardDef; count: number }>>((acc, card) => {
    const existing = acc.find((entry) => entry.card.art === card.art)
    if (existing) existing.count += 1
    else acc.push({ card, count: 1 })
    return acc
  }, [])
})

const attachedCards = (card: Arkham.CardDef) => props.attachments[card.art] ?? []

const groupedAttachedCards = (card: Arkham.CardDef) => {
  return attachedCards(card).reduce<Array<{ card: Arkham.CardDef; count: number }>>((acc, attached) => {
    const existing = acc.find((entry) => entry.card.art === attached.art)
    if (existing) existing.count += 1
    else acc.push({ card: attached, count: 1 })
    return acc
  }, [])
}

const underworldMarketCards = () => props.attachments['09077'] ?? []

const marketCardCount = (card: Arkham.CardDef) => underworldMarketCards().filter((c) => c.art === card.art).length

const marketTooltip = (card: Arkham.CardDef) => `Attached to Market deck (x ${marketCardCount(card)})`

const isUnderworldMarketCard = (card: Arkham.CardDef) => marketCardCount(card) > 0

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`
  return `${card.name.title}${subtitle}`
}
</script>

<template>
  <div class="cards">
    <div
      v-for="{ card, count } in groupedCards"
      :key="card.art"
      class="card-tile"
      :class="{ 'has-attachments': attachedCards(card).length > 0 }"
    >
      <a target="_blank" :href="`${localizeArkhamDBBaseUrl()}/card/${card.art}`">
        <CardImage :card="card" />
        <span class="card-badges">
          <span v-if="showCounts" class="deck-card-count">x {{ count }}</span>
          <span v-if="isUnderworldMarketCard(card)" class="market-badge" v-tooltip="marketTooltip(card)" :aria-label="marketTooltip(card)">
            <font-awesome-icon icon="store" />
            <span>x {{ marketCardCount(card) }}</span>
          </span>
        </span>
      </a>
      <div v-if="attachedCards(card).length > 0" class="attachments-panel">
        <div class="attachments-title"><font-awesome-icon icon="paperclip" /> Attached cards</div>
        <div class="attachment-grid">
          <a
            v-for="entry in groupedAttachedCards(card)"
            :key="entry.card.art"
            class="attachment-card"
            target="_blank"
            :href="`${localizeArkhamDBBaseUrl()}/card/${entry.card.art}`"
            :title="cardName(entry.card)"
          >
            <CardImage :card="entry.card" />
            <span class="attachment-label">
              <span class="attachment-name">{{ cardName(entry.card) }}</span>
              <span class="attachment-count">x {{ entry.count }}</span>
            </span>
          </a>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.cards {
  --min-col-width: 200px;
  flex: 1;
  overflow-y: auto;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(min(var(--min-col-width), 100%), 1fr));
  gap: 12px;
  padding: 16px;
  align-content: start;

  &:deep(.card-container) {
    width: 100%;
    max-width: unset;
    margin: 0;
  }
}

.card-tile {
  display: flex;
  flex-direction: column;
  gap: 8px;
  align-self: start;

  > a {
    position: relative;
    display: flex;
    justify-content: center;
  }
}

.card-badges {
  position: absolute;
  left: 10px;
  bottom: 10px;
  z-index: 1;
  display: inline-flex;
  align-items: center;
  gap: 5px;
}

.deck-card-count {
  display: inline-grid;
  place-items: center;
  min-width: 30px;
  height: 26px;
  padding: 0 7px;
  color: #d7d7d7;
  background: rgba(0, 0, 0, 0.72);
  border: 1px solid rgba(255, 255, 255, 0.18);
  border-radius: 7px;
  box-shadow: 0 2px 7px rgba(0, 0, 0, 0.45);
  font-size: 0.82rem;
  font-weight: 800;
  white-space: nowrap;
}

.market-badge {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 5px;
  min-width: 30px;
  height: 26px;
  padding: 0 7px;
  color: #c8a96e;
  background: rgba(0, 0, 0, 0.72);
  border: 1px solid rgba(200, 169, 110, 0.46);
  border-radius: 7px;
  box-shadow: 0 2px 7px rgba(0, 0, 0, 0.45);
  font-size: 0.82rem;
  font-weight: 800;
}

.has-attachments {
  grid-column: 1 / -1;
  display: grid;
  grid-template-columns: minmax(180px, 240px) 1fr;
  align-items: start;
  padding: 10px;
  background: linear-gradient(180deg, rgba(200, 169, 110, 0.12), rgba(255, 255, 255, 0.035));
  border: 1px solid rgba(200, 169, 110, 0.24);
  border-radius: 12px;
  box-shadow: 0 8px 22px rgba(0, 0, 0, 0.28);

  @media (max-width: 768px) {
    grid-template-columns: 1fr;
  }
}

.attachments-panel {
  padding: 8px;
  background: rgba(0, 0, 0, 0.26);
  border: 1px solid rgba(255, 255, 255, 0.07);
  border-radius: 9px;
}

.attachments-title {
  display: flex;
  align-items: center;
  gap: 6px;
  margin-bottom: 7px;
  color: #c8a96e;
  font-size: 0.68rem;
  font-weight: 900;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.attachment-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
  gap: 8px;
}

.attachment-card {
  min-width: 0;
  color: #f0e2c0;
  text-decoration: none;
  font-size: 0.68rem;
  font-weight: 700;

  &:deep(.card-container) {
    border-radius: 5px;
    overflow: hidden;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.35);
  }

  .attachment-label {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 5px;
    margin-top: 3px;
  }

  .attachment-name {
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .attachment-count {
    flex: 0 0 auto;
    padding: 1px 5px;
    color: #1d170f;
    background: #c8a96e;
    border-radius: 999px;
    font-size: 0.58rem;
    font-weight: 900;
    white-space: nowrap;
  }

  &:hover { opacity: 0.82; }
}
</style>
