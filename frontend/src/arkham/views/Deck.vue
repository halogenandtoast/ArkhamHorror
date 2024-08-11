<script lang="ts" setup>
import { watch, ref, computed, onMounted } from 'vue';
import { useRouter } from 'vue-router'
import { fetchDeck, deleteDeck, fetchCards, syncDeck } from '@/arkham/api';
import { imgsrc, investigatorClass } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';
import type {Deck} from '@/arkham/types/Deck';
import Prompt from '@/components/Prompt.vue'
import { useToast } from "vue-toastification";

import sets from '@/arkham/data/sets.json'

export interface Props {
  deckId: string
}

const props = defineProps<Props>()
const router = useRouter()
const toast = useToast()
const allCards = ref<Arkham.CardDef[]>([])
const ready = ref(false)
const deleting = ref(false)
const deck = ref<Deck | null>(null)
const deckRef = ref(null)

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
    if (a.art < b.art) {
      return -1
    }

    if (a.art > b.art) {
      return 1
    }

    return 0
  })
  fetchDeck(props.deckId).then((deckData) => {
    deck.value = deckData
    ready.value = true
  })
})

const image = (card: Arkham.CardDef) => imgsrc(`cards/${card.art}.jpg`)
const view = ref(View.List)

const cards = computed(() => {
  if (deck.value === undefined || deck.value === null) {
    return []
  }

  return Object.entries(deck.value.list.slots).flatMap(([key, value]) => {
    if (key === "c01000") {
      return Array(value).fill({ cardCode: key, classSymbols: [], cardType: "Treachery", art: "01000", level: 0, name: { title: "Random Basic Weakness", subtitle: null }, cardTraits: [], skills: [], cost: null })
    }
    const result = allCards.value.find((c) => `c${c.art}` === key)
    return Array(value).fill(result)
  })
})

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`

  return `${card.name.title}${subtitle}`
}

const cardCost = (card: Arkham.CardDef) => {
  if (card.cost?.tag === "StaticCost") {
    return card.cost.contents
  }

  if (card.cost?.tag === "DynamicCost") {
    return -2
  }

  if (card.cost?.tag === "DiscardAmountCost") {
    return -2
  }

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

  const set = cardSet(card)

  if (set !== null && set !== undefined) {
    return `${set.name} ${setNumber % 500}`
  }

  return "Unknown"
}

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
  // converts https://arkhamdb.com/api/public/decklist/25027
  // to https://arkhamdb.com/decklist/view/25027
  // OR
  // converts https://arkhamdb.com/api/public/deck/25027
  // to https://arkhamdb.com/deck/view/25027
  return url.replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
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
  if (deckInvestigator.value) {
    return investigatorClass(deckInvestigator.value)
  }

  return {};
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
          <img v-if="deckInvestigator" class="portrait--decklist" :src="imgsrc(`cards/${deckInvestigator}.jpg`)" />
          <div class="deck--details">
            <h1 class="deck-title">{{deck.name}}</h1>
            <div class="deck--actions">
              <div class="deck--view-options">
                <button @click.prevent="view = View.List" :class="{ pressed: view == View.List }">
                  <font-awesome-icon icon="list" />
                </button>
                <button @click.prevent="view = View.Image" :class="{ pressed: view == View.Image }">
                  <font-awesome-icon icon="image" />
                </button>
              </div>
              <div class="deck--non-view-options">
                <div class="open-deck">
                  <a v-if="deck.url" :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener"><font-awesome-icon alt="View Deck in ArkhamDB" icon="external-link" /></a>
                </div>
                <div v-if="deck.url" class="sync-deck">
                  <a href="#" @click.prevent="sync"><font-awesome-icon icon="refresh" /></a>
                </div>
                <div class="deck-delete">
                  <a href="#delete" @click.prevent="deleting = true"><font-awesome-icon icon="trash" /></a>
                </div>
              </div>
            </div>
          </div>
        </template>
      </header>
      <div class="cards" v-if="view == View.Image">
        <img class="card" v-for="(card, idx) in cards" :key="idx" :src="image(card)" />
      </div>
      <table class="list" v-if="view == View.List">
        <thead>
          <tr><th>Name</th><th>Class</th><th>Cost</th><th>Type</th><th>Icons</th><th>Traits</th><th>Set</th></tr>
        </thead>
        <tbody>
          <tr v-for="(card, idx) in cards" :key="idx">
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
    <Prompt
      v-if="deleting"
      prompt="Are you sure you want to delete this deck?"
      :yes="deleteDeckEvent"
      :no="() => deleting = false"
    />
  </div>
</template>

<style scoped lang="scss">
.container {
  display: flex;
  min-width: 60vw;
  margin: 0 auto;
  margin-top: 20px;
}

.results {
  flex: 1;
}

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
  box-sizing: border-box;
}

table {
  width: calc(100% - 40px);
  box-sizing: border-box;
  padding: 20px 0;
  margin: 20px;
  border-radius: 10px;
  background-color: rgba(255,255,255,0.9);
  border-collapse: collapse;
}

th {
  text-align: left;
}

tr td:nth-child(1){
  padding-left: 20px;
}

tr th:nth-child(1){
  padding-left: 20px;
}

tbody td {
  padding: 2px 5px;
}

thead tr th {
  background-color: #111;
  color: #aaa;
  padding: 5px 5px;

  &:nth-child(1) {
    border-top-left-radius: 10px;
  }

  &:last-child {
    border-top-right-radius: 10px;
  }
}

tr {
  color: #cecece;
}

tr:nth-child(odd) {
  background-color: #333;
}

tr:nth-child(even) {
  background-color: #222;
}

.willpower {
  color: $guardian;
}

.intellect {
  color: $mystic;
}

.combat {
  color: $survivor;
}

.agility {
  color: $rogue;
}

.wild {
  color: $seeker;
}

a {
  font-weight: bold;
  color: $spooky-green;
  text-decoration: none;
  &:hover {
    color: $spooky-green-light;
  }
}

.cycles {
  color: #999;
  overflow-y: auto;
}

button {
  border: 0;
}

i {
  font-style: normal;
}

.pressed {
  background-color: #333;
  color: white;
}

/* deck header */

.open-deck {
  justify-self: flex-end;
  align-self: flex-start;
  margin-right: 10px;
}

.sync-deck {
  justify-self: flex-end;
  align-self: flex-start;
  margin-right: 10px;
}

.deck-delete {
  justify-self: flex-end;
  align-self: flex-start;
  a {
    color: var(--title);
    &:hover {
      color: #990000;
    }
  }
}

.portrait--decklist {
  width: 200px;
  margin-right: 10px;
  border-radius: 10px;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.deck-title {
  font-weight: 800;
  font-size: 2em;
  margin: 0;
  padding: 0;
  flex: 1;
}

.deck {
  background-color: #15192C;
  color: #f0f0f0;
  margin: 0 20px;
  padding: 20px;

  &.guardian {
    background-color: var(--guardian-dark);
    background: linear-gradient(30deg, var(--guardian-extra-dark), var(--guardian-dark));
  }

  &.seeker {
    background: linear-gradient(30deg, var(--seeker-extra-dark), var(--seeker-dark));
  }

  &.rogue {
    background: linear-gradient(30deg, var(--rogue-extra-dark), var(--rogue-dark));
  }

  &.mystic {
    background: linear-gradient(30deg, var(--mystic-extra-dark), var(--mystic-dark));
  }

  &.survivor {
    background: linear-gradient(30deg, var(--survivor-extra-dark), var(--survivor-dark));
  }

  &.neutral {
    background-color: var(--neutral-dark);
    background-image: linear-gradient(30deg, var(--neutral-extra-dark), var(--neutral-dark));
  }

  /*&.survivor::after {
    content: '';
    display: block;
    width: 100%;
    height: 100%;
    position: absolute;
    inset: 0;
    background: #0000000d;
    mask-position: left top;
    mask-size: cover;
    -webkit-mask-image: url(/img/arkham/masks/survivor.svg);
    mask-image: url(/img/arkham/masks/survivor.svg);
    z-index: -1;
  }*/
  a {
    color: var(--title);
    font-weight: bolder;
    &:hover {
      color: rgba(0, 0, 0, 0.4);
    }
  }

  display: flex;

  width: calc(100% - 40px);
  box-sizing: border-box;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  border-radius: 10px;
  &.is-pinned {
    border-top-left-radius: 0;
    border-top-right-radius: 0;
  }
  background-color: rgba(0,0,0,0.3);

  color: white;

  input {
    margin-right: 10px;
  }

  button {
    padding: 5px;
  }

  position: sticky;
  position: -webkit-sticky;
  top: -1px;

  &.is-pinned {
    background-color: rgba(28, 28, 41, 1) !important;
  }
}

.deck--details {
  flex: 1;
  display: flex;
  flex-direction: column;
}

.deck--actions {
  display: flex;
}

.deck--view-options {
  display: flex;
  flex: 1;
}

.deck--non-view-options {
  display: flex;
  align-self: flex-end;
}
</style>
