<script lang="ts" setup>
import { computed } from 'vue'
import { useRouter } from 'vue-router'
import { displayTabooId } from '@/arkham/taboo';
import {imgsrc, localizeArkhamDBBaseUrl, investigatorClass} from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/Deck'

interface Props {
  deck: Arkham.Deck
  sync?: () => void
  markDelete?: () => void
}

const props = defineProps<Props>()
const router = useRouter()

function navigateToDeck() {
  router.push({ name: 'Deck', params: { deckId: props.deck.id } })
}

const deckUrlToPage = (url: string): string => {
  return url
    .replace("https://arkhamdb.com", localizeArkhamDBBaseUrl())
    .replace("/api/public/decklist", "/decklist/view")
    .replace("/api/public/deck", "/deck/view")
}

const deckInvestigator = computed(() => {
  if (props.deck.list.meta) {
    try {
      const result = JSON.parse(props.deck.list.meta)
      if (result && result.alternate_front) {
        return result.alternate_front
      }
    } catch (e) { console.log("No parse") }
  }
  return props.deck.list.investigator_code.replace('c', '')
})

const deckClass = computed(() => {
  if (deckInvestigator.value) {
    return investigatorClass(deckInvestigator.value)
  }
  return {};
})

const tabooList = computed(() => {
  return props.deck.list.taboo_id ? displayTabooId(props.deck.list.taboo_id) : null
})
</script>

<template>
  <div class="decklist box" :class="deckClass" @click="navigateToDeck">
    <img class="portrait--decklist" :src="imgsrc(`cards/${deckInvestigator}.avif`)" />
    <div class="deck-details">
      <div class="deck-main">
        <span class="deck-name">{{ deck.name }}</span>
        <span v-if="tabooList" class="taboo-badge"><font-awesome-icon icon="book" /> Taboo: {{ tabooList }}</span>
      </div>
      <div class="deck-actions" @click.stop>
        <a v-if="deck.url" class="action-btn" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener" title="View on ArkhamDB">
          <font-awesome-icon icon="external-link" />
        </a>
        <a v-if="deck.url && sync" class="action-btn" href="#" title="Sync deck" @click.prevent="sync">
          <font-awesome-icon icon="refresh" />
        </a>
        <a v-if="markDelete" class="action-btn action-btn--delete" href="#" title="Delete deck" @click.prevent="markDelete">
          <font-awesome-icon icon="trash" />
        </a>
      </div>
    </div>
  </div>
</template>

<style scoped>
.decklist {
  display: flex;
  gap: 16px;
  color: #f0f0f0;
  border-left: 4px solid transparent;
  cursor: pointer;
  transition: background-color 0.2s, border-color 0.2s;

  &.guardian { border-left-color: var(--guardian-dark); &:hover { background-color: var(--guardian-extra-dark); .deck-name { color: oklch(from var(--guardian-dark) calc(l + 0.2) c h); } } }
  &.seeker   { border-left-color: var(--seeker-dark);   &:hover { background-color: var(--seeker-extra-dark);   .deck-name { color: oklch(from var(--seeker-dark)   calc(l + 0.2) c h); } } }
  &.rogue    { border-left-color: var(--rogue-dark);    &:hover { background-color: var(--rogue-extra-dark);    .deck-name { color: oklch(from var(--rogue-dark)    calc(l + 0.2) c h); } } }
  &.mystic   { border-left-color: var(--mystic-dark);   &:hover { background-color: var(--mystic-extra-dark);   .deck-name { color: oklch(from var(--mystic-dark)   calc(l + 0.2) c h); } } }
  &.survivor { border-left-color: var(--survivor-dark); &:hover { background-color: var(--survivor-extra-dark); .deck-name { color: oklch(from var(--survivor-dark) calc(l + 0.2) c h); } } }
  &.neutral  { border-left-color: var(--neutral-dark);  &:hover { background-color: var(--neutral-extra-dark);  .deck-name { color: oklch(from var(--neutral-dark)  calc(l + 0.2) c h); } } }
}

.portrait--decklist {
  width: 150px;
  border-radius: 5px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  flex-shrink: 0;
  align-self: flex-start;
  transition: transform 0.3s ease;

  .decklist:hover & {
    transform: scale(1.04);
  }
}

.deck-details {
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  flex: 1;
  min-width: 0;
  padding: 4px 0;
}

.deck-main {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.deck-name {
  font-size: 1.2em;
  font-weight: 800;
  color: var(--title);
  line-height: 1.2;
  transition: color 0.15s;
}


.taboo-badge {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  width: fit-content;
  padding: 1px 7px;
  line-height: 1.6;
  font-size: 0.75em;
  font-weight: 600;
  color: #c8a96e;
  background: rgba(200, 169, 110, 0.12);
  border: 1px solid rgba(200, 169, 110, 0.25);
  border-radius: 4px;
  letter-spacing: 0.02em;
}

.deck-actions {
  display: flex;
  align-items: center;
  gap: 14px;
}

.action-btn {
  color: #8a93a8;
  font-size: 0.9em;
  text-decoration: none;
  transition: color 0.15s;

  &:hover { color: #fff; }
  &.action-btn--delete { &:hover { color: #ff6666; } }
}
</style>
