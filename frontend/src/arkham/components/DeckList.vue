<script lang="ts" setup>
import { watch, ref, computed, onMounted } from 'vue';
import { imgsrc, localizeArkhamDBBaseUrl } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';
import type { Deck} from '@/arkham/types/Deck';
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'
import { useCardStore } from '@/stores/cards'
import { storeToRefs } from 'pinia';
import sets from '@/arkham/data/sets.json'

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

const allCards = computed(() => {
  return Object.entries(props.deck.list.slots).flatMap(([key, value]) => {
    if (key === "c01000") {
      return Array(value).fill({ cardCode: key, classSymbols: [], cardType: "Treachery", art: "01000", level: 0, name: { title: "Random Basic Weakness", subtitle: null }, cardTraits: [], skills: [], cost: null })
    }

    const result: Arkham.CardDef | undefined = cards.value.find((c) => `c${c.art}` === key)
    const language = localStorage.getItem('language') || 'en'
    if (language === 'en') return Array(value).fill(result)

    if (!result) return
    const match: ArkhamDBCard | null = store.getDbCard(result.art)
    if (!match) return Array(value).fill(result)

    result.name.title = match.name
    if (match.subname) result.name.subtitle = match.subname
    if (match.faction_name && result.classSymbols.length > 0) result.classSymbols[0] = match.faction_name
    if (match.faction2_name && result.classSymbols.length > 1) {
      result.classSymbols[1] = match.faction2_name
      if (match.faction3_name && result.classSymbols.length > 2) result.classSymbols[2] = match.faction3_name
    }

    result.cardType = match.type_name
    if (match.traits) result.cardTraits = match.traits.split('.').filter(item => item != "" && item != " ")

    return Array(value).fill(result)
  })
})

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
              <a v-if="deck.url" class="action-btn" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener" title="View on ArkhamDB">
                <font-awesome-icon icon="external-link" />
              </a>
            </div>
          </div>
        </template>
      </header>
      <div class="cards" v-if="view == View.Image">
        <img class="card" v-for="(card, idx) in allCards" :key="idx" :src="image(card)" />
      </div>
      <table class="card-table" v-if="view == View.List">
        <thead>
          <tr><th>Name</th><th>Class</th><th>Cost</th><th>Type</th><th>Icons</th><th>Traits</th><th>Set</th></tr>
        </thead>
        <tbody>
          <tr v-for="(card, idx) in allCards" :key="idx">
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

/* ── Skill icons ─────────────────────────────────────────── */

i { font-style: normal; }

.willpower { font-size: 1.3em; margin: 0 1px; color: var(--willpower); }
.intellect { font-size: 1.3em; margin: 0 1px; color: var(--intellect); }
.combat    { font-size: 1.3em; margin: 0 1px; color: var(--combat); }
.agility   { font-size: 1.3em; margin: 0 1px; color: var(--agility); }
.wild      { font-size: 1.3em; margin: 0 1px; color: var(--wild); }
</style>
