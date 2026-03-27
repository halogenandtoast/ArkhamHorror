<script lang="ts" setup>
import { watch, shallowRef, ref, computed, onMounted } from 'vue';
import { useRouter } from 'vue-router'
import { fetchDeck, deleteDeck, fetchCards, syncDeck } from '@/arkham/api';
import { imgsrc, localizeArkhamDBBaseUrl } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';
import type {Deck} from '@/arkham/types/Deck';
import * as DeckHelpers from '@/arkham/types/Deck';
import Prompt from '@/components/Prompt.vue'
import CardListView from '@/arkham/components/CardListView.vue'
import CardImageView from '@/arkham/components/CardImageView.vue'
import { useToast } from "vue-toastification";
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'
import { displayTabooId } from '@/arkham/taboo'

export interface Props {
  deckId: string
}

const props = defineProps<Props>()
const router = useRouter()
const toast = useToast()
const allCards = shallowRef<Arkham.CardDef[]>([])
const ready = ref(false)
const deleting = ref(false)
const deck = shallowRef<Deck | null>(null)
const deckRef = ref(null)
const store = useDbCardStore()

onMounted(() => {
  if (deckRef.value !== null) {
    const el = deckRef.value
    const observer = new IntersectionObserver(
      ([e]) => e.target.classList.toggle("is-pinned", e.intersectionRatio < 1),
      { threshold: [1] }
    );

    observer.observe(el);
  }
})

const enum View {
  Image = "IMAGE",
  List = "LIST",
}

fetchCards(true).then((response) => {
  allCards.value = response.sort((a, b) => {
    if (a.art < b.art) return -1
    if (a.art > b.art) return 1
    return 0
  })

  fetchDeck(props.deckId).then((deckData) => {
    deck.value = deckData
    ready.value = true
  })
})

const view = ref(View.List)

const cards = computed(() => {
  if (deck.value === undefined || deck.value === null) {
    return []
  }

  return Object.entries(deck.value.list.slots).flatMap(([key, value]) => {
    if (key === "c01000") {
      return Array(value).fill({ cardCode: key, classSymbols: [], cardType: "Treachery", art: "01000", level: 0, name: { title: "Random Basic Weakness", subtitle: null }, cardTraits: [], skills: [], cost: null })
    }

    const result: Arkham.CardDef | undefined = allCards.value.find((c) => `c${c.art}` === key)
    const language = localStorage.getItem('language') || 'en'
    if (language === 'en') return Array(value).fill(result)

    if (!result) return
    const match: ArkhamDBCard | null = store.getDbCard(result.art)
    if (!match) return Array(value).fill(result)

    // Name
    result.name.title = match.name
    if (match.subname) result.name.subtitle = match.subname

    // Class
    if (match.faction_name && result.classSymbols.length > 0) result.classSymbols[0] = match.faction_name
    if (match.faction2_name && result.classSymbols.length > 1) {
      result.classSymbols[1] = match.faction2_name
      if (match.faction3_name && result.classSymbols.length > 2) result.classSymbols[2] = match.faction3_name
    }

    // Type
    result.cardType = match.type_name

    // Traits
    if (match.traits) result.cardTraits = match.traits.split('.').filter(item => item != "" && item != " ")

    return Array(value).fill(result)
  })
})

async function deleteDeckEvent() {
  if (deck.value) {
    deleteDeck(deck.value.id).then(() => {
      router.push({ name: 'Decks' })
    })
  }
}

async function sync() {
  if (deck.value) {
    syncDeck(deck.value.id).then((newData) => {
      toast.success("Deck synced successfully", { timeout: 3000 })
      deck.value = newData
    })
  }
}

const deckUrlToPage = (url: string): string => {
  return url.replace("https://arkhamdb.com", localizeArkhamDBBaseUrl()).replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
}

const deckInvestigator = computed(() => {
  if (deck.value) {
    if (deck.value.list.meta) {
      try {
        const result = JSON.parse(deck.value.list.meta)
        if (result && result.alternate_front) {
          return result.alternate_front
        }
      } catch (e) { console.log("No parse") }
    }
    return deck.value.list.investigator_code.replace('c', '')
  }

  return null
})

const deckClass = computed(() => {
  if (deck.value) return DeckHelpers.deckClass(deck.value)
  return {}
})

const tabooList = computed(() => {
  return deck.value?.list.taboo_id ? displayTabooId(deck.value.list.taboo_id) : null
})

watch(deckRef, (el) => {
  if (el !== null) {
    const observer = new IntersectionObserver(
      ([e]) => e.target.classList.toggle("is-pinned", e.intersectionRatio < 1),
      { threshold: [1] }
    );

    observer.observe(el);
  }
})

</script>

<template>
  <div class="container">
    <div class="results">
      <header class="deck" v-show="deck" ref="deckRef" :class="deckClass">
        <template v-if="deck">
          <img v-if="deckInvestigator" class="portrait--decklist" :src="imgsrc(`cards/${deckInvestigator}.avif`)" />
          <div class="deck--details">
            <div class="deck-main">
              <h1 class="deck-title">{{deck.name}}</h1>
              <span v-if="tabooList" class="taboo-badge"><font-awesome-icon icon="book" /> Taboo: {{ tabooList }}</span>
            </div>
            <div class="deck--actions">
              <div class="deck--view-options">
                <button @click.prevent="view = View.List" :class="{ pressed: view == View.List }">
                  <font-awesome-icon icon="list" />
                </button>
                <button @click.prevent="view = View.Image" :class="{ pressed: view == View.Image }">
                  <font-awesome-icon icon="image" />
                </button>
              </div>
              <div class="deck-actions">
                <a v-if="deck.url" class="action-btn" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener" title="View on ArkhamDB"><font-awesome-icon icon="external-link" /></a>
                <a v-if="deck.url" class="action-btn" href="#" title="Sync deck" @click.prevent="sync"><font-awesome-icon icon="refresh" /></a>
                <a class="action-btn action-btn--delete" href="#" title="Delete deck" @click.prevent="deleting = true"><font-awesome-icon icon="trash" /></a>
              </div>
            </div>
          </div>
        </template>
      </header>
      <CardImageView v-if="view == View.Image" :cards="cards" />
      <CardListView v-if="view == View.List" :cards="cards" />
    </div>
    <Prompt
      v-if="deleting"
      prompt="Are you sure you want to delete this deck?"
      :yes="deleteDeckEvent"
      :no="() => deleting = false"
    />
  </div>
</template>

<style scoped>
/* ── Layout ─────────────────────────────────────────────── */

.container {
  display: flex;
  height: calc(100vh - 40px);
  max-width: unset;
  margin: 0;
  overflow: hidden;
}

.results {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

/* ── Deck header ─────────────────────────────────────────── */

.deck {
  display: flex;
  gap: 16px;
  padding: 20px;
  color: #f0f0f0;
  background: var(--box-background);
  border-left: 4px solid transparent;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  position: sticky;
  position: -webkit-sticky;
  top: -1px;
  flex-shrink: 0;

  &.guardian { border-left-color: var(--guardian-dark); background: linear-gradient(30deg, var(--guardian-extra-dark), var(--guardian-dark)); }
  &.seeker   { border-left-color: var(--seeker-dark);   background: linear-gradient(30deg, var(--seeker-extra-dark), var(--seeker-dark)); }
  &.rogue    { border-left-color: var(--rogue-dark);    background: linear-gradient(30deg, var(--rogue-extra-dark), var(--rogue-dark)); }
  &.mystic   { border-left-color: var(--mystic-dark);   background: linear-gradient(30deg, var(--mystic-extra-dark), var(--mystic-dark)); }
  &.survivor { border-left-color: var(--survivor-dark); background: linear-gradient(30deg, var(--survivor-extra-dark), var(--survivor-dark)); }
  &.neutral  { border-left-color: var(--neutral-dark);  background: linear-gradient(30deg, var(--neutral-extra-dark), var(--neutral-dark)); }

  &.is-pinned {
    background: rgba(28, 28, 41, 1) !important;
    border-left-color: transparent !important;
  }
}

.portrait--decklist {
  width: 200px;
  flex-shrink: 0;
  align-self: flex-start;
  border-radius: 10px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.deck--details {
  flex: 1;
  display: flex;
  flex-direction: column;
}

.deck-main {
  display: flex;
  flex-direction: column;
  gap: 8px;
  flex: 1;
}

.deck-title {
  font-weight: 800;
  font-size: 2em;
  margin: 0;
  padding: 0;
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

.deck--actions {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-top: auto;
}

.deck--view-options {
  display: flex;
  gap: 2px;
  background: rgba(255,255,255,0.05);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 6px;
  padding: 2px;
  width: fit-content;

  button {
    background: transparent;
    border: none;
    border-radius: 4px;
    padding: 5px 9px;
    color: #777;
    cursor: pointer;
    transition: background 0.12s, color 0.12s;

    &:hover { color: #ccc; }
    &.pressed { background: rgba(255,255,255,0.12); color: #eee; }
  }
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
  &.action-btn--delete:hover { color: #ff6666; }
}
</style>
