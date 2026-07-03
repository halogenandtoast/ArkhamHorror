<script lang="ts" setup>
import { computed } from 'vue'
import * as Arkham from '@/arkham/types/CardDef'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import CardImage from '@/arkham/components/CardImage.vue'

const props = withDefaults(defineProps<{ cards: Arkham.CardDef[], attachments?: Record<string, Arkham.CardDef[]>, showCounts?: boolean }>(), {
  attachments: () => ({}),
  showCounts: true,
})

const ungroupedWarOfTheOuterGodsCards = new Set(['c86038a', 'c86044a', 'c86049a'])

const groupKey = (card: Arkham.CardDef) => ungroupedWarOfTheOuterGodsCards.has(card.cardCode) ? card.cardCode : card.art

const groupCards = (cards: Arkham.CardDef[]) => {
  const grouped = new Map<string, { card: Arkham.CardDef; count: number }>()

  for (const card of cards) {
    const key = groupKey(card)
    const existing = grouped.get(key)
    if (existing) existing.count += 1
    else grouped.set(key, { card, count: 1 })
  }

  return Array.from(grouped.values())
}

const groupedCards = computed(() => groupCards(props.cards))

const attachedCards = (card: Arkham.CardDef) => props.attachments[card.art] ?? []

const groupedAttachedCards = (card: Arkham.CardDef) => groupCards(attachedCards(card))

const underworldMarketCards = () => props.attachments['09077'] ?? []
const spiritDeckCards = () => props.attachments['90052'] ?? []
const stickToThePlanCards = () => props.attachments['03264'] ?? []
const ancestralKnowledgeCards = () => props.attachments['07303'] ?? []
const bewitchingCards = () => props.attachments['10079'] ?? []
const eldritchBrandCards = () => props.attachments['11080'] ?? []

const countCards = (cards: Arkham.CardDef[]) => {
  const counts = new Map<string, number>()
  for (const card of cards) counts.set(card.art, (counts.get(card.art) ?? 0) + 1)
  return counts
}

const marketCardCounts = computed(() => countCards(underworldMarketCards()))
const spiritCardCounts = computed(() => countCards(spiritDeckCards()))
const stickToThePlanCardCounts = computed(() => countCards(stickToThePlanCards()))
const ancestralKnowledgeCardCounts = computed(() => countCards(ancestralKnowledgeCards()))
const bewitchingCardCounts = computed(() => countCards(bewitchingCards()))
const eldritchBrandCardCounts = computed(() => countCards(eldritchBrandCards()))

const marketCardCount = (card: Arkham.CardDef) => marketCardCounts.value.get(card.art) ?? 0
const spiritCardCount = (card: Arkham.CardDef) => spiritCardCounts.value.get(card.art) ?? 0
const stickToThePlanCardCount = (card: Arkham.CardDef) => stickToThePlanCardCounts.value.get(card.art) ?? 0
const ancestralKnowledgeCardCount = (card: Arkham.CardDef) => ancestralKnowledgeCardCounts.value.get(card.art) ?? 0
const bewitchingCardCount = (card: Arkham.CardDef) => bewitchingCardCounts.value.get(card.art) ?? 0
const eldritchBrandCardCount = (card: Arkham.CardDef) => eldritchBrandCardCounts.value.get(card.art) ?? 0

const marketTooltip = (card: Arkham.CardDef) => `Attached to Market deck (x ${marketCardCount(card)})`
const spiritTooltip = (card: Arkham.CardDef) => `In Spirit deck (x ${spiritCardCount(card)})`
const stickToThePlanTooltip = (card: Arkham.CardDef) => `Attached to Stick to the Plan (x ${stickToThePlanCardCount(card)})`
const ancestralKnowledgeTooltip = (card: Arkham.CardDef) => `Attached to Ancestral Knowledge (x ${ancestralKnowledgeCardCount(card)})`
const bewitchingTooltip = (card: Arkham.CardDef) => `Attached to Bewitching (x ${bewitchingCardCount(card)})`
const eldritchBrandTooltip = (card: Arkham.CardDef) => `Branded by Eldritch Brand (x ${eldritchBrandCardCount(card)})`

const isUnderworldMarketCard = (card: Arkham.CardDef) => marketCardCount(card) > 0
const isSpiritDeckCard = (card: Arkham.CardDef) => spiritCardCount(card) > 0
const isStickToThePlanCard = (card: Arkham.CardDef) => stickToThePlanCardCount(card) > 0
const isAncestralKnowledgeCard = (card: Arkham.CardDef) => ancestralKnowledgeCardCount(card) > 0
const isBewitchingCard = (card: Arkham.CardDef) => bewitchingCardCount(card) > 0
const isEldritchBrandCard = (card: Arkham.CardDef) => eldritchBrandCardCount(card) > 0

const attachmentTitle = (card: Arkham.CardDef) => {
  if (card.art === '90052') return 'Spirit deck'
  if (card.art === '09077') return 'Underworld Market'
  if (card.art === '11080') return 'Eldritch Brand'
  return 'Attached cards'
}

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`
  return `${card.name.title}${subtitle}`
}
</script>

<template>
  <div class="cards">
    <div
      v-for="{ card, count } in groupedCards"
      :key="groupKey(card)"
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
          <span v-if="isStickToThePlanCard(card)" class="market-badge" v-tooltip="stickToThePlanTooltip(card)" :aria-label="stickToThePlanTooltip(card)">
            <font-awesome-icon icon="paperclip" />
            <span>x {{ stickToThePlanCardCount(card) }}</span>
          </span>
          <span v-if="isAncestralKnowledgeCard(card)" class="market-badge" v-tooltip="ancestralKnowledgeTooltip(card)" :aria-label="ancestralKnowledgeTooltip(card)">
            <font-awesome-icon icon="paperclip" />
            <span>x {{ ancestralKnowledgeCardCount(card) }}</span>
          </span>
          <span v-if="isBewitchingCard(card)" class="market-badge" v-tooltip="bewitchingTooltip(card)" :aria-label="bewitchingTooltip(card)">
            <font-awesome-icon icon="paperclip" />
            <span>x {{ bewitchingCardCount(card) }}</span>
          </span>
          <span v-if="isEldritchBrandCard(card)" class="market-badge" v-tooltip="eldritchBrandTooltip(card)" :aria-label="eldritchBrandTooltip(card)">
            <font-awesome-icon icon="book" />
            <span>x {{ eldritchBrandCardCount(card) }}</span>
          </span>
          <span v-if="isSpiritDeckCard(card)" class="spirit-badge" v-tooltip="spiritTooltip(card)" :aria-label="spiritTooltip(card)">
            <font-awesome-icon :icon="['fas', 'ghost']" />
            <span>x {{ spiritCardCount(card) }}</span>
          </span>
        </span>
      </a>
      <div v-if="attachedCards(card).length > 0" class="attachments-panel">
        <div class="attachments-title" :class="{ 'attachments-title--spirit': card.art === '90052' }">
          <font-awesome-icon :icon="card.art === '90052' ? ['fas', 'ghost'] : 'paperclip'" /> {{ attachmentTitle(card) }}
        </div>
        <div class="attachment-grid">
          <a
            v-for="entry in groupedAttachedCards(card)"
            :key="groupKey(entry.card)"
            class="attachment-card"
            target="_blank"
            :href="`${localizeArkhamDBBaseUrl()}/card/${entry.card.art}`"
            :title="cardName(entry.card)"
          >
            <CardImage :card="entry.card" />
            <span class="attachment-label">
              <span class="attachment-name">{{ cardName(entry.card) }}{{ card.art === '11080' ? ' was branded' : '' }}</span>
              <span v-if="card.art !== '11080' || entry.count > 1" class="attachment-count">x {{ entry.count }}</span>
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
  z-index: var(--z-index-1);
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

.market-badge,
.spirit-badge {
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

.spirit-badge {
  color: #b8d7ff;
  border-color: rgba(120, 170, 255, 0.5);
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

.attachments-title--spirit {
  color: #b8d7ff;
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
