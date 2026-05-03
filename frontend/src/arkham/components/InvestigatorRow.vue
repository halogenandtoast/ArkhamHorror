<script lang="ts" setup>
import * as Arkham from '@/arkham/types/Game'
import { ref, computed } from 'vue';
import { Investigator } from '@/arkham/types/Investigator';
import Card from '@/arkham/components/Card.vue'
import DeckList from '@/arkham/components/DeckList.vue'
import {imgsrc} from '@/arkham/helpers'
import { useDbCardStore } from '@/stores/dbCards'
import type { CardContents } from '@/arkham/types/Card';

export interface Props {
  investigator: Investigator
  game: Arkham.Game
  bonusXp?: number | null;
  showExpand?: boolean;
}

const store = useDbCardStore()
const props = withDefaults(defineProps<Props>(), {
  bonusXp: null,
  showExpand: true,
})

const expanded = ref(false)

const storyCards = computed(() => {
  const fromMeta = props.game.campaign?.meta?.otherCampaignAttrs?.storyCards[props.investigator.id]
  if (fromMeta) {
    return fromMeta.map((c) => ({tag: 'CardContents', ...c} as CardContents))
  }

  return props.game.campaign?.storyCards[props.investigator.id] || props.game.scenario?.storyCards[props.investigator.id] || []
})

function getInvestigatorName(cardTitle: string): string {
  const language = localStorage.getItem('language') || 'en'
  return language === 'en'? cardTitle : store.getCardName(cardTitle, "investigator")
}

const deck = computed(() => {
  const deck = props.game.campaign?.decks[props.investigator.id] || props.game.campaign?.meta?.otherCampaignAttrs?.decks[props.investigator.id]

  if (!deck) return null
  const slots = deck.reduce((acc, { cardCode }) => {
      acc[cardCode] = (acc[cardCode] ?? 0) + 1;
      return acc;
    }, {});
  return {
    id: props.investigator.id,
    name: "",
    url: props.investigator.deckUrl,
    list: {
      investigator_code: props.investigator.id,
      taboo_id: null,
      meta: null,
      slots
    }
  }
})
</script>

<template>
  <div class="investigator" :class="investigator.class.toLowerCase()">
    <div class="basic">
      <div class="basic-top">
        <div class="portrait-wrap" :class="investigator.class.toLowerCase()">
          <img :src="imgsrc(`portraits/${investigator.id.replace('c', '')}.jpg`)" class="investigator-portrait"/>
        </div>
        <span class="name">{{ getInvestigatorName(investigator.name.title) }}</span>
        <slot name="back" :investigator="props.investigator">
          <button v-if="showExpand" class="expand-btn" @click="expanded = !expanded" :aria-label="expanded ? 'Collapse' : 'Expand'">
            <svg class="icon icon-expand" :class="{ expanded }"><use xlink:href="#icon-right-arrow"></use></svg>
          </button>
        </slot>
      </div>
      <div class="basic-bottom">
        <div class="stat-chip stat-xp">
          <span class="stat-label">XP</span>
          <span class="stat-value">{{ investigator.xp }}<span v-if="bonusXp" class="bonus-xp"> +{{ bonusXp }}</span></span>
        </div>
        <div class="stat-chip stat-health">
          <svg class="icon icon-health"><use xlink:href="#icon-health"></use></svg>
          <span class="stat-label">Physical</span>
          <span class="stat-value">{{ investigator.physicalTrauma }}</span>
        </div>
        <div class="stat-chip stat-sanity">
          <svg class="icon icon-sanity"><use xlink:href="#icon-sanity"></use></svg>
          <span class="stat-label">Mental</span>
          <span class="stat-value">{{ investigator.mentalTrauma }}</span>
        </div>
      </div>
    </div>

    <div v-if="expanded" class="expanded-details">
      <section v-if="storyCards.length > 0" class="inner-section">
        <h2>Earned Cards</h2>
        <div class="earned-cards">
          <div v-for="card in storyCards">
            <Card :game="game" :card="card" :playerId="investigator.id" />
          </div>
        </div>
      </section>

      <div v-if="deck" class="deck-section">
        <DeckList :deck="deck" embedded />
      </div>
    </div>
  </div>
</template>

<style scoped>
/* ── Container ───────────────────────────────────────────── */

.investigator {
  min-width: 80%;
  border-radius: 10px;
  overflow: hidden;

  &.guardian { background: var(--guardian-extra-dark); }
  &.seeker   { background: var(--seeker-extra-dark); }
  &.rogue    { background: var(--rogue-extra-dark); }
  &.mystic   { background: var(--mystic-extra-dark); }
  &.survivor { background: var(--survivor-extra-dark); }
  &.neutral  { background: var(--neutral-extra-dark); }
}

/* ── Basic row ───────────────────────────────────────────── */

.basic {
  display: flex;
  flex-direction: column;
  color: #f0f0f0;
}

.basic-top {
  display: flex;
  align-items: center;
  gap: 14px;
  padding: 10px 14px;
}

.basic-bottom {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-wrap: wrap;
  padding: 8px 14px;
  border-top: 1px solid rgba(255,255,255,0.1);
}

/* ── Portrait ────────────────────────────────────────────── */

.portrait-wrap {
  width: 52px;
  height: 52px;
  border-radius: 6px;
  overflow: hidden;
  flex-shrink: 0;
  box-shadow: 0 2px 8px rgba(0,0,0,0.5);
  border: 2px solid transparent;

  &.guardian { border-color: var(--guardian); }
  &.seeker   { border-color: var(--seeker); }
  &.rogue    { border-color: var(--rogue); }
  &.mystic   { border-color: var(--mystic); }
  &.survivor { border-color: var(--survivor); }
  &.neutral  { border-color: var(--neutral); }
}

.investigator-portrait {
  width: 150px;
  display: block;
}

/* ── Name ────────────────────────────────────────────────── */

.name {
  flex: 1;

  font-family: teutonic, sans-serif;
  font-size: 1.4em;
  color: #f0f0f0;
  letter-spacing: 0.04em;
  line-height: 1.1;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}


/* ── Expand button ───────────────────────────────────────── */

.expand-btn {
  background: transparent;
  border: none;
  color: rgba(255,255,255,0.4);
  cursor: pointer;
  padding: 6px;
  border-radius: 4px;
  transition: color 0.15s, background 0.15s;
  flex-shrink: 0;

  &:hover { color: #fff; background: rgba(255,255,255,0.08); }
}

.icon {
  display: inline-block;
  width: 1.2em;
  height: 1.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}

.expand-btn .icon {
  width: 1.8em;
  height: 1.8em;
}

.icon-expand {
  transform: rotate(0deg);
  transition: transform 0.25s ease;
  &.expanded { transform: rotate(90deg); }
}

/* ── Expanded details ────────────────────────────────────── */

.expanded-details {
  border-top: 1px solid rgba(255,255,255,0.08);
  display: flex;
  flex-direction: column;
  gap: 0;
  padding: 12px 0;
}

/* ── Stat chips ──────────────────────────────────────────── */


.stat-chip {
  display: flex;
  align-items: center;
  gap: 5px;
  padding: 5px 10px;
  border-radius: 6px;
  background: rgba(0,0,0,0.25);
  border: 1px solid rgba(255,255,255,0.08);

  .icon { width: 1em; height: 1em; }
}

.stat-label {
  font-size: 0.72em;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: rgba(255,255,255,0.45);
}

.stat-value {
  font-size: 0.9em;
  font-weight: 700;
  color: #f0f0f0;
}

.stat-xp .stat-value { color: #a8d080; }
.stat-health .icon   { color: #f88; }
.stat-sanity .icon   { color: #8af; }

.bonus-xp {
  font-size: 0.8em;
  color: rgba(168, 208, 128, 0.6);
}

/* ── Earned cards ────────────────────────────────────────── */

.inner-section {
  margin: 0 14px 10px;
  background: rgba(0,0,0,0.3);
  border: 1px solid rgba(255,255,255,0.06);
  padding: 12px 14px;
  border-radius: 8px;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.2em;
    color: rgba(255,255,255,0.6);
    text-transform: uppercase;
    letter-spacing: 0.08em;
    margin: 0 0 10px;
  }

  :deep(.card) { width: 10vw; }
}

.earned-cards {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 10px;
}

/* ── Deck section ────────────────────────────────────────── */

.deck-section {
  border-top: 1px solid rgba(255,255,255,0.06);

  :deep(.deck) {
    background: rgba(0,0,0,0.25);
    box-shadow: none;

    &.is-pinned {
      background: rgba(0,0,0,0.75) !important;
    }
  }

  :deep(.card-table thead th) {
    background: rgba(0,0,0,0.65);
    color: rgba(255,255,255,0.8);
    border-bottom-color: rgba(255,255,255,0.08);
  }

  :deep(.card-table tbody tr) {
    border-bottom-color: rgba(255,255,255,0.04);
    color: rgba(255,255,255,0.75);
    &:hover { background: rgba(0,0,0,0.15); }
  }

  :deep(.cards) {
    grid-template-columns: repeat(auto-fill, minmax(min(200px, calc(33.333% - 8px)), 1fr));
  }
}
</style>
