<script lang="ts" setup>
import { watch, shallowRef, ref, computed, onMounted } from 'vue';
import { useRouter } from 'vue-router'
import { fetchDeck, deleteDeck, fetchCards, syncDeck, setDeckOverlay, removeDeckOverlay } from '@/arkham/api';
import { storeToRefs } from 'pinia'
import { useSettings } from '@/stores/settings'
import OverlayEditor, { type DeckOverlay } from '@/arkham/components/debug/OverlayEditor.vue'
import { customCardDef, isCustomCardCode, stripCardCodePrefix } from '@/arkham/customCards'
import { loadLibrary } from '@/arkham/customCardLibrary'
import { cardImg, localizeArkhamDBBaseUrl } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';
import type {Deck} from '@/arkham/types/Deck';
import * as DeckHelpers from '@/arkham/types/Deck';
import Prompt from '@/components/Prompt.vue'
import CardListView from '@/arkham/components/CardListView.vue'
import CardImageView from '@/arkham/components/CardImageView.vue'
import { useToast } from "vue-toastification";
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'
import { displayTabooId } from '@/arkham/taboo'
import { useI18n } from 'vue-i18n'

const { t } = useI18n()

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

/* An overlay saved here sticks to the deck: it is applied whenever the deck is
 * played, and removing it leaves the deck exactly as it was. */
const { customCardsEnabled } = storeToRefs(useSettings())
const overlayOpen = ref(false)
const overlay = ref<DeckOverlay | null>(null)
const savingOverlay = ref(false)

function openOverlay() {
  overlay.value = ((deck.value as any)?.overlay ?? null) as DeckOverlay | null
  overlayOpen.value = !overlayOpen.value
}

async function saveOverlay() {
  if (!deck.value) return
  savingOverlay.value = true
  try {
    if (overlay.value) await setDeckOverlay(deck.value.id, overlay.value)
    else await removeDeckOverlay(deck.value.id)
    deck.value = await fetchDeck(deck.value.id)
    overlayOpen.value = false
    toast.success(overlay.value ? 'Overlay applied' : 'Overlay removed')
  } catch (e) {
    console.error(e)
    toast.error('Could not save the overlay')
  } finally {
    savingOverlay.value = false
  }
}
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

// Custom cards resolve out of your library, which the deck may name.
if (customCardsEnabled.value) loadLibrary()

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

function localizeCard(result: Arkham.CardDef | undefined): Arkham.CardDef | undefined {
  if (!result) return undefined

  const language = localStorage.getItem('language') || 'en'
  if (language === 'en') return result

  const match: ArkhamDBCard | null = store.getDbCard(result.art)
  if (!match) return result

  const localized = { ...result, name: { ...result.name }, classSymbols: [...result.classSymbols], cardTraits: [...result.cardTraits] }

  // Name
  localized.name.title = match.name
  if (match.subname) localized.name.subtitle = match.subname

  // Class
  if (match.faction_name && localized.classSymbols.length > 0) localized.classSymbols[0] = match.faction_name
  if (match.faction2_name && localized.classSymbols.length > 1) {
    localized.classSymbols[1] = match.faction2_name
    if (match.faction3_name && localized.classSymbols.length > 2) localized.classSymbols[2] = match.faction3_name
  }

  // Type
  localized.cardType = match.type_name

  // Traits
  if (match.traits) localized.cardTraits = match.traits.split('.').filter(item => item != "" && item != " ")

  return localized
}

function findCardByDeckCode(code: string): Arkham.CardDef | undefined {
  if (code === "c01000") {
    return { cardCode: code, doubleSided: false, classSymbols: [], cardType: "Treachery", art: "01000", level: 0, name: { title: "Random Basic Weakness", subtitle: null }, cardTraits: [], skills: [], cost: null, otherSide: null, meta: {}, errata: null }
  }

  // A card you built has no entry in the server's card list; it resolves
  // through your library instead.
  if (isCustomCardCode(code)) return customCardDef(stripCardCodePrefix(code))

  const normalized = code.replace(/^c/, '')
  return localizeCard(allCards.value.find((c) => c.art === normalized))
}

const cardsFromSlots = (slots: Record<string, number> | undefined): Arkham.CardDef[] => {
  if (!slots) return []

  return Object.entries(slots).flatMap(([key, value]) => {
    const result = findCardByDeckCode(key)
    if (!result) return []
    return Array(value).fill(result)
  })
}

const cardsFromList = (codes: string): Arkham.CardDef[] => {
  return codes
    .split(',')
    .map((code) => findCardByDeckCode(code.trim()))
    .filter((card): card is Arkham.CardDef => !!card)
}

/* Everything shown is the deck as it will be played, so an overlay is visible
 * here rather than only taking effect at the table. */
const playList = computed(() => (deck.value ? DeckHelpers.deckPlayList(deck.value) : null))

const deckMeta = computed<Record<string, unknown>>(() => {
  try {
    return playList.value?.meta ? JSON.parse(playList.value.meta) as Record<string, unknown> : {}
  } catch (_e) {
    return {}
  }
})

const hasFromTheBeyond = computed(() => {
  return !!playList.value?.slots['90052'] || !!playList.value?.slots['c90052']
})

const withoutCards = (source: Arkham.CardDef[], cardsToRemove: Arkham.CardDef[]) => {
  const remaining = new Map<string, number>()
  cardsToRemove.forEach((card) => remaining.set(card.cardCode, (remaining.get(card.cardCode) ?? 0) + 1))

  return source.filter((card) => {
    const count = remaining.get(card.cardCode) ?? 0
    if (count <= 0) return true
    remaining.set(card.cardCode, count - 1)
    return false
  })
}

const cards = computed(() => withoutCards(cardsFromSlots(playList.value?.slots), hunchDeckCards.value))

const hunchDeckCards = computed(() => {
  if (!deck.value) return []
  const investigatorCode = (playList.value?.investigator_code ?? '').replace(/^c/, '')
  if (investigatorCode !== '05002') return []
  const hunchCards = deckMeta.value[`attachments_${investigatorCode}`]
  return typeof hunchCards === 'string' ? cardsFromList(hunchCards) : []
})

const hasTrait = (card: Arkham.CardDef, traitName: string) => {
  return card.cardTraits.some((trait) => trait.toLowerCase() === traitName.toLowerCase())
}

const isSpiritDeckCard = (card: Arkham.CardDef) => {
  return hasTrait(card, 'Ally') || hasTrait(card, 'Geist') || hasTrait(card, 'Spirit')
}

const attachmentLimits: Record<string, number> = {
  '03264': 3, // Stick to the Plan
  '07303': 5, // Ancestral Knowledge
  '10079': 3, // Bewitching
}

const hasCardInDeck = (code: string) => {
  const slots = playList.value?.slots ?? {}
  return !!slots[code] || !!slots[`c${code}`]
}

const sideSlotCards = computed(() => cardsFromSlots(playList.value?.sideSlots))
const explicitAttachmentCards = computed(() => {
  return Object.entries(attachmentLimits).flatMap(([code, limit]) => {
    if (!hasCardInDeck(code)) return []
    const value = deckMeta.value[`attachments_${code}`]
    return typeof value === 'string' ? cardsFromList(value).slice(0, limit) : []
  })
})
const sideSlotCardsWithoutAttachments = computed(() => withoutCards(sideSlotCards.value, explicitAttachmentCards.value))

const sideSlotCardsAreSpiritDeck = computed(() => {
  return hasFromTheBeyond.value && sideSlotCardsWithoutAttachments.value.length > 0 && sideSlotCardsWithoutAttachments.value.every(isSpiritDeckCard)
})

const sideDeckCards = computed(() => sideSlotCardsAreSpiritDeck.value ? [] : sideSlotCardsWithoutAttachments.value)

const attachments = computed<Record<string, Arkham.CardDef[]>>(() => {
  const result: Record<string, Arkham.CardDef[]> = {}

  Object.entries(deckMeta.value).forEach(([key, value]) => {
    const match = key.match(/^attachments_(\d+)$/)
    if (!match || typeof value !== 'string') return

    const code = match[1]
    const attachedCards = cardsFromList(value).slice(0, attachmentLimits[code] ?? undefined)
    if (attachedCards.length > 0) result[code] = attachedCards
  })

  const hiddenSlots = (deckMeta.value.hidden_slots as { slots?: Record<string, number> } | undefined)?.slots
  if (hiddenSlots) {
    const hiddenCards = cardsFromSlots(hiddenSlots)
    if (hiddenCards.length > 0) {
      if (hasFromTheBeyond.value && !result['90052']) result['90052'] = hiddenCards
      else if (!result['09077']) result['09077'] = hiddenCards
    }
  }

  if (sideSlotCardsAreSpiritDeck.value) {
    result['90052'] = [...(result['90052'] ?? []), ...sideSlotCardsWithoutAttachments.value]
  }

  if (typeof deckMeta.value.extra_deck === 'string' && hasFromTheBeyond.value) {
    const extraCards = cardsFromList(deckMeta.value.extra_deck)
    if (extraCards.length > 0) result['90052'] = [...(result['90052'] ?? []), ...extraCards]
  }

  return result
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
      toast.success(t('deckSyncedSuccessfully'), { timeout: 3000 })
      deck.value = newData
    })
  }
}

const deckUrlToPage = (url: string): string => {
  return url.replace("https://arkhamdb.com", localizeArkhamDBBaseUrl()).replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
}

// An overlay that swaps the investigator swaps the face of the deck with it,
// which the play list already reflects.
const deckInvestigator = computed(() =>
  deck.value ? DeckHelpers.deckInvestigator(deck.value) : null
)

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
          <img v-if="deckInvestigator" class="portrait--decklist" :src="cardImg(deckInvestigator)" />
          <div class="deck--details">
            <div class="deck-main">
              <h1 class="deck-title">{{deck.name}}</h1>
              <span v-if="tabooList" class="taboo-badge"><font-awesome-icon icon="book" /> Taboo: {{ tabooList }}</span>
            </div>
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
              <a v-if="deck.url" class="action-btn" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener" :title="$t('deck.viewOnArkhamDb')"><font-awesome-icon icon="external-link" /></a>
              <a v-if="deck.url" class="action-btn" href="#" :title="$t('deck.syncDeck')" @click.prevent="sync"><font-awesome-icon icon="refresh" /></a>
              <a v-if="customCardsEnabled" class="action-btn" href="#" title="Overlay" @click.prevent="openOverlay"><font-awesome-icon icon="layer-group" /></a>
              <a class="action-btn action-btn--delete" href="#" :title="$t('deck.deleteDeck')" @click.prevent="deleting = true"><font-awesome-icon icon="trash" /></a>
            </div>
            <div v-if="overlayOpen" class="overlay-panel">
              <h3>Overlay</h3>
              <p class="overlay-help">
                Your own cards, laid over this deck. The deck's own list is kept, so removing the
                overlay puts it back exactly as it was.
              </p>
              <OverlayEditor v-model="overlay" :slots="deck.list.slots" :investigator="deck.list.investigator_code" />
              <div class="overlay-actions">
                <button type="button" :disabled="savingOverlay" @click="saveOverlay">
                  {{ overlay ? 'Apply overlay' : 'Remove overlay' }}
                </button>
                <button type="button" @click="overlayOpen = false">Cancel</button>
              </div>
            </div>
          </div>
        </template>
      </header>
      <div class="deck-sections">
        <section v-if="hunchDeckCards.length > 0" class="deck-section deck-section--hunch">
          <h2 class="deck-section-title">Hunch Deck <span>{{ hunchDeckCards.length }}</span></h2>
          <CardImageView v-if="view == View.Image" :cards="hunchDeckCards" />
          <CardListView v-if="view == View.List" :cards="hunchDeckCards" />
        </section>

        <section class="deck-section deck-section--main">
          <h2 class="deck-section-title">Main Deck <span>{{ cards.length }}</span></h2>
          <CardImageView v-if="view == View.Image" :cards="cards" :attachments="attachments" />
          <CardListView v-if="view == View.List" :cards="cards" :attachments="attachments" />
        </section>

        <section v-if="sideDeckCards.length > 0" class="deck-section deck-section--side">
          <h2 class="deck-section-title">Side Deck <span>{{ sideDeckCards.length }}</span></h2>
          <CardImageView v-if="view == View.Image" :cards="sideDeckCards" />
          <CardListView v-if="view == View.List" :cards="sideDeckCards" />
        </section>
      </div>
    </div>
    <Prompt
      v-if="deleting"
      :prompt="t('areYouSureDeleteDeck')"
      :yes="deleteDeckEvent"
      :no="() => deleting = false"
    />
  </div>
</template>

<style scoped>
/* ── Layout ─────────────────────────────────────────────── */

.container {
  display: flex;
  height: calc(100vh - var(--nav-height));
  max-width: unset;
  margin: 0;
  overflow: hidden;
  @media (max-width: 768px) {
    height: auto;
    overflow-x: hidden;
    overflow-y: visible;
    flex-direction: column;
  }
}

.results {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  @media (max-width: 768px) {
    overflow: visible;
  }
}

.deck-sections {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  padding: 14px 16px 24px;
}

.deck-section {
  display: flex;
  flex-direction: column;
  min-height: 0;
  margin-bottom: 18px;
  background: rgba(255, 255, 255, 0.025);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 12px;
  overflow: hidden;

  &:last-child { margin-bottom: 0; }

  &:deep(.cards),
  &:deep(.card-table-wrapper) {
    flex: unset;
    overflow: visible;
  }
}

.deck-section-title {
  display: flex;
  align-items: center;
  gap: 8px;
  margin: 0;
  padding: 10px 14px;
  color: #e8dfc9;
  background: rgba(0, 0, 0, 0.28);
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
  font-size: 0.82rem;
  font-weight: 900;
  letter-spacing: 0.08em;
  text-transform: uppercase;

  span {
    padding: 1px 7px;
    color: #1d170f;
    background: #c8a96e;
    border-radius: 999px;
    font-size: 0.68rem;
    letter-spacing: 0;
  }
}

.deck-section--hunch .deck-section-title { color: #b8d7ff; }
.deck-section--side .deck-section-title { color: #d2c6ff; }

/* ── Deck header ─────────────────────────────────────────── */

.deck {
  --deck-pad: 20px;
  display: flex;
  flex-wrap: wrap;
  column-gap: 16px;
  row-gap: 0;
  padding: var(--deck-pad) 0 0; /* no horizontal padding — children handle their own spacing */
  color: #f0f0f0;
  background: var(--box-background);
  border-left: 4px solid transparent;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  position: sticky;
  position: -webkit-sticky;
  top: -1px;
  flex-shrink: 0;
  align-items: flex-start;
  @media (max-width: 768px) {
    --deck-pad: 12px;
    position: static;
    padding: 10px 0 0;
  }

  &.guardian { border-left-color: var(--guardian-dark); }
  &.seeker   { border-left-color: var(--seeker-dark); }
  &.rogue    { border-left-color: var(--rogue-dark); }
  &.mystic   { border-left-color: var(--mystic-dark); }
  &.survivor { border-left-color: var(--survivor-dark); }
  &.neutral  { border-left-color: var(--neutral-dark); }
}

.portrait--decklist {
  width: 200px;
  flex-shrink: 0;
  align-self: flex-start;
  border-radius: 10px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  margin-left: var(--deck-pad);
  @media (max-width: 768px) {
    width: 72px;
    border-radius: 6px;
  }
}

.deck--details {
  flex: 1;
  display: flex;
  flex-direction: column;
  min-width: 0;
  margin-right: var(--deck-pad);
  margin-bottom: var(--deck-pad);
}

.deck-main {
  display: flex;
  flex-direction: column;
  gap: 8px;
  flex: 1;
  @media (max-width: 768px) {
    min-width: 0;
    gap: 4px;
  }
}

.deck-title {
  font-weight: 800;
  font-size: 2em;
  margin: 0;
  padding: 0;
  @media (max-width: 768px) {
    font-size: 1.1em;
  }
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
  gap: 16px;
  flex-basis: 100%;
  padding: 8px var(--deck-pad);
  background: rgba(0, 0, 0, 0.2);
  border-top: 1px solid rgba(255, 255, 255, 0.08);
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

.overlay-panel {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  margin-top: 0.75rem;
  padding: 0.75rem;
  width: 100%;

  h3 {
    font-family: teutonic, sans-serif;
    font-size: 1.1em;
    margin: 0 0 0.25rem;
  }
}

.overlay-help {
  font-size: 0.85rem;
  margin: 0 0 0.5rem;
  opacity: 0.75;
}

.overlay-actions {
  display: flex;
  gap: 0.5rem;
  margin-top: 0.75rem;

  button {
    background: rgba(255, 255, 255, 0.08);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    cursor: pointer;
    padding: 0.35rem 0.7rem;
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
