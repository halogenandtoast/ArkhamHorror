<script lang="ts" setup>
import * as Arkham from '@/arkham/types/CardDef'
import { localizeArkhamDBBaseUrl } from '@/arkham/helpers'
import CardImage from '@/arkham/components/CardImage.vue'

const props = withDefaults(defineProps<{ cards: Arkham.CardDef[], attachments?: Record<string, Arkham.CardDef[]> }>(), {
  attachments: () => ({}),
})

const attachedCards = (card: Arkham.CardDef) => props.attachments[card.art] ?? []

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`
  return `${card.name.title}${subtitle}`
}
</script>

<template>
  <div class="cards">
    <div
      v-for="(card, idx) in cards"
      :key="idx"
      class="card-tile"
      :class="{ 'has-attachments': attachedCards(card).length > 0 }"
    >
      <a target="_blank" :href="`${localizeArkhamDBBaseUrl()}/card/${card.art}`">
        <CardImage :card="card" />
      </a>
      <div v-if="attachedCards(card).length > 0" class="attachments-panel">
        <div class="attachments-title"><font-awesome-icon icon="paperclip" /> Attached cards</div>
        <div class="attachment-grid">
          <a
            v-for="(attached, attachedIdx) in attachedCards(card)"
            :key="`${attached.art}-${attachedIdx}`"
            class="attachment-card"
            target="_blank"
            :href="`${localizeArkhamDBBaseUrl()}/card/${attached.art}`"
            :title="cardName(attached)"
          >
            <CardImage :card="attached" />
            <span>{{ cardName(attached) }}</span>
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
    display: flex;
    justify-content: center;
  }
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

  span {
    display: block;
    margin-top: 3px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  &:hover { opacity: 0.82; }
}
</style>
