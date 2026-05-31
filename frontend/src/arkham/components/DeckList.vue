<script lang="ts" setup>
import { watch, ref, computed, onMounted } from 'vue';
import { imgsrc, localizeArkhamDBBaseUrl } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';
import type { Deck} from '@/arkham/types/Deck';
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'
import { useCardStore } from '@/stores/cards'
import { storeToRefs } from 'pinia';
import sets from '@/arkham/data/sets.json'
import CardImage from '@/arkham/components/CardImage.vue'

const props = withDefaults(defineProps<{ deck: Deck; embedded?: boolean }>(), { embedded: false })
const deckRef = ref(null)
const store = useDbCardStore()
const cardStore = useCardStore()
const { cards } = storeToRefs(cardStore)

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

const image = (card: Arkham.CardDef) => imgsrc(`cards/${card.art}.avif`)
const view = ref(View.List)

function localizeCard(result: Arkham.CardDef | undefined): Arkham.CardDef | undefined {
  if (!result) return undefined

  const language = localStorage.getItem('language') || 'en'
  if (language === 'en') return result

  const match: ArkhamDBCard | null = store.getDbCard(result.art)
  if (!match) return result

  const localized = { ...result, name: { ...result.name }, classSymbols: [...result.classSymbols], cardTraits: [...result.cardTraits] }
  localized.name.title = match.name
  if (match.subname) localized.name.subtitle = match.subname
  if (match.faction_name && localized.classSymbols.length > 0) localized.classSymbols[0] = match.faction_name
  if (match.faction2_name && localized.classSymbols.length > 1) {
    localized.classSymbols[1] = match.faction2_name
    if (match.faction3_name && localized.classSymbols.length > 2) localized.classSymbols[2] = match.faction3_name
  }

  localized.cardType = match.type_name
  if (match.traits) localized.cardTraits = match.traits.split('.').filter(item => item != "" && item != " ")

  return localized
}

function findCardByDeckCode(code: string): Arkham.CardDef | undefined {
  if (code === "c01000") {
    return { cardCode: code, doubleSided: false, classSymbols: [], cardType: "Treachery", art: "01000", level: 0, name: { title: "Random Basic Weakness", subtitle: null }, cardTraits: [], skills: [], cost: null, otherSide: null, meta: {} }
  }

  const normalized = code.replace(/^c/, '')
  return localizeCard(cards.value.find((c) => c.art === normalized))
}

const allCards = computed(() => {
  return Object.entries(props.deck.list.slots).flatMap(([key, value]) => {
    const result = findCardByDeckCode(key)
    if (!result) return []
    return Array(value).fill(result)
  })
})

const attachments = computed<Record<string, Arkham.CardDef[]>>(() => {
  if (!props.deck.list.meta) return {}

  try {
    const meta = JSON.parse(props.deck.list.meta) as Record<string, unknown>
    return Object.entries(meta).reduce<Record<string, Arkham.CardDef[]>>((acc, [key, value]) => {
      const match = key.match(/^attachments_(\d+)$/)
      if (!match || typeof value !== 'string') return acc

      const attachedCards = value
        .split(',')
        .map((code) => findCardByDeckCode(code.trim()))
        .filter((card): card is Arkham.CardDef => !!card)

      if (attachedCards.length > 0) acc[match[1]] = attachedCards
      return acc
    }, {})
  } catch (_e) {
    return {}
  }
})

const attachedCards = (card: Arkham.CardDef) => attachments.value[card.art] ?? []

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`

  return `${card.name.title}${subtitle}`
}

const cardCost = (card: Arkham.CardDef) => {
  if (card.cost?.tag === "StaticCost") return card.cost.contents
  if (card.cost?.tag === "DynamicCost") return -2
  if (card.cost?.tag === "DeferredCost") return -2
  if (card.cost?.tag === "DiscardAmountCost") return -2

  return null
}

const cardType = (card: Arkham.CardDef) => {
  switch(card.cardType) {
    case "PlayerTreacheryType":
      return "Treachery"
    case "PlayerEnemyType":
      return "Enemy"
    default:
      return card.cardType.replace(/Type$/, '')
  }
}

const cardTraits = (card: Arkham.CardDef) => {
  if (card.cardTraits.length === 0) { return '' }
  return `${card.cardTraits.join('. ')}.`
}

const levelText = (card: Arkham.CardDef) => {
  if (card.level === 0 || card.level === null) return ''
  return ` (${card.level})`
}

const cardIcons = (card: Arkham.CardDef) => {
  return card.skills.map((s) => {
    if(s.tag === "SkillIcon") {
      switch(s.contents) {
        case "SkillWillpower": return "willpower"
        case "SkillIntellect": return "intellect"
        case "SkillCombat": return "combat"
        case "SkillAgility": return "agility"
        default: return "unknown"
      }
    }

    if (s.tag == "WildIcon" || s.tag == "WildMinusIcon") {
      return "wild"
    }

    return "unknown"
  })
}

const cardSet = (card: Arkham.CardDef) => {
  const cardCode = parseInt(card.art)
  return sets.find((s) => cardCode >= s.min && cardCode <= s.max)
}

const cardSetText = (card: Arkham.CardDef) => {
  const setNumber = parseInt(card.art.slice(2,))
  const language = localStorage.getItem('language') || 'en'
  var setName = ''

  if (language !== 'en') {
    const match: ArkhamDBCard | null = store.getDbCard(card.art)
    if (match) setName = match.pack_name
  }

  if (!setName) {
    const set = cardSet(card)
    if (set !== null && set !== undefined) setName = set.name
  }

  if (setName) return `${setName} ${setNumber % 500}`
  else return "Unknown"
}

const deckUrlToPage = (url: string): string => {
  return url.replace("https://arkhamdb.com", localizeArkhamDBBaseUrl()).replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
}

const deckInvestigator = computed(() => {
  if (props.deck) {
    if (props.deck.list.meta) {
      try {
        const result = JSON.parse(props.deck.list.meta)
        if (result && result.alternate_front) {
          return result.alternate_front
        }
      } catch (e) { console.log("No parse") }
    }
    return props.deck.list.investigator_code.replace('c', '')
  }

  return null
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
  <div class="container" :class="{ embedded }">
    <div class="results">
      <header class="deck" v-show="deck" ref="deckRef">
        <template v-if="deck">
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
              <a v-if="deck.url" class="action-btn" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener" :title="$t('deck.viewOnArkhamDb')">
                <font-awesome-icon icon="external-link" />
              </a>
            </div>
          </div>
        </template>
      </header>
      <div class="cards" v-if="view == View.Image">
        <div
          v-for="(card, idx) in allCards"
          :key="idx"
          class="card-tile"
          :class="{ 'has-attachments': attachedCards(card).length > 0 }"
        >
          <img class="card" :src="image(card)" />
          <div v-if="attachedCards(card).length > 0" class="attachments-panel">
            <div class="attachments-title"><font-awesome-icon icon="paperclip" /> Attached cards for {{ cardName(card) }}</div>
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
      <table class="card-table" v-if="view == View.List">
        <thead>
          <tr><th>{{ $t('cardsList.name') }}</th><th>{{ $t('cardsList.class') }}</th><th>{{ $t('cardsList.cost') }}</th><th>{{ $t('cardsList.type') }}</th><th>{{ $t('cardsList.icons') }}</th><th>{{ $t('cardsList.traits') }}</th><th>{{ $t('cardsList.set') }}</th></tr>
        </thead>
        <tbody>
          <template v-for="(card, idx) in allCards" :key="idx">
            <tr>
              <td>{{cardName(card)}}{{levelText(card)}}</td>
              <td>{{card.classSymbols.join(', ')}}</td>
              <td>{{cardCost(card)}}</td>
              <td>{{cardType(card)}}</td>
              <td>
                <i v-for="(icon, index) in cardIcons(card)" :key="index" :class="[icon, `${icon}-icon`]" ></i>
              </td>
              <td>{{cardTraits(card)}}</td>
              <td>{{cardSetText(card)}}</td>
            </tr>
            <tr v-if="attachedCards(card).length > 0" class="attachments-row">
              <td colspan="7">
                <div class="attachments-list">
                  <div class="attachments-heading">
                    <font-awesome-icon icon="paperclip" /> Attached cards for {{ cardName(card) }}
                  </div>
                  <div class="attachment-pills">
                    <a
                      v-for="(attached, attachedIdx) in attachedCards(card)"
                      :key="`${attached.art}-${attachedIdx}`"
                      class="attachment-pill"
                      target="_blank"
                      :href="`${localizeArkhamDBBaseUrl()}/card/${attached.art}`"
                    >
                      <span class="attachment-count">{{ attachedIdx + 1 }}</span>
                      {{ cardName(attached) }}{{ levelText(attached) }}
                    </a>
                  </div>
                </div>
              </td>
            </tr>
          </template>
        </tbody>
      </table>
    </div>
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
  width: 100%;

  &.embedded {
    height: auto;
    overflow: visible;
  }
}

.results {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  min-width: 0;

  .embedded & {
    overflow: visible;
  }
}

.embedded .card-table {
  display: table;
  overflow-y: visible;
  height: auto;
}

/* ── Deck header ─────────────────────────────────────────── */

.deck {
  display: flex;
  padding: 12px 20px;
  color: #f0f0f0;
  background: #1a1a1a;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  position: sticky;
  position: -webkit-sticky;
  top: -1px;
  flex-shrink: 0;
}

.deck--actions {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
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
}

/* ── Card table ──────────────────────────────────────────── */

.card-table {
  display: block;
  overflow-y: auto;
  flex: 1;
  width: 100%;
  border-collapse: collapse;
  font-size: 0.86rem;

  thead {
    position: sticky;
    top: 0;
    z-index: 1;

    th {
      text-align: left;
      padding: 8px 10px;
      font-size: 0.75rem;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: #666;
      background: var(--box-background);
      border-bottom: 1px solid rgba(255,255,255,0.06);

      &:first-child { padding-left: 20px; }
    }
  }

  tbody tr {
    color: #cecece;
    border-bottom: 1px solid rgba(255,255,255,0.04);
    transition: background 0.1s;

    &:hover { background: rgba(255,255,255,0.04); }

    td {
      padding: 6px 10px;
      &:first-child { padding-left: 20px; }
    }
  }
}

.attachments-row td {
  padding-top: 0 !important;
  padding-bottom: 10px !important;
  background: rgba(200, 169, 110, 0.035);
}

.attachments-list {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 7px;
  width: 100%;
  padding: 9px 10px;
  background: linear-gradient(135deg, rgba(200, 169, 110, 0.14), rgba(255, 255, 255, 0.035));
  border: 1px solid rgba(200, 169, 110, 0.24);
  border-radius: 9px;
  box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.04);
}

.attachments-heading {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  color: #c8a96e;
  font-size: 0.68rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.attachment-pills {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
}

.attachment-pill {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  padding: 3px 8px 3px 4px;
  color: #f0e2c0;
  background: rgba(0, 0, 0, 0.28);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 999px;
  font-size: 0.74rem;
  font-weight: 600;
  text-decoration: none;
  &:hover { background: rgba(200, 169, 110, 0.16); opacity: 1; }
}

.attachment-count {
  display: inline-grid;
  place-items: center;
  width: 16px;
  height: 16px;
  color: #1d170f;
  background: #c8a96e;
  border-radius: 50%;
  font-size: 0.62rem;
  font-weight: 900;
}

/* ── Image view ──────────────────────────────────────────── */

.card {
  width: calc(100% - 20px);
  margin: 10px;
  border-radius: 10px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.cards {
  overflow-y: auto;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
  padding: 10px;
}

.card-tile {
  display: flex;
  flex-direction: column;
  gap: 8px;
  align-self: start;
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
    width: 100%;
    max-width: unset;
    margin: 0;
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

/* ── Skill icons ─────────────────────────────────────────── */

i { font-style: normal; }

.willpower { font-size: 1.3em; margin: 0 1px; color: var(--willpower); }
.intellect { font-size: 1.3em; margin: 0 1px; color: var(--intellect); }
.combat    { font-size: 1.3em; margin: 0 1px; color: var(--combat); }
.agility   { font-size: 1.3em; margin: 0 1px; color: var(--agility); }
.wild      { font-size: 1.3em; margin: 0 1px; color: var(--wild); }
</style>
