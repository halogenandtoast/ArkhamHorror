<script lang="ts" setup>
import { ref, computed } from 'vue';
import { useRouter } from 'vue-router'
import { fetchDeck, deleteDeck, fetchCards, syncDeck } from '@/arkham/api';
import { imgsrc } from '@/arkham/helpers';
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

enum View {
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
  if (card.level === 0) return ''
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
</script>

<template>
  <div class="container">
    <div class="results">
      <header class="deck" v-if="deck">
        <img class="portrait--decklist" :src="imgsrc(`cards/${deck.list.investigator_code.replace('c', '')}.jpg`)" />
        <div class="deck--details">
          <h1 class="deck-title">{{deck.name}}</h1>
          <div class="deck--actions">
            <div class="deck--view-options">
              <button @click.prevent="view = View.List" :class="{ pressed: view == View.List }"><font-awesome-icon icon="list" /></button>
              <button @click.prevent="view = View.Image" :class="{ pressed: view == View.Image }"><font-awesome-icon icon="image" /></button>
            </div>
            <div class="deck--non-view-options">
              <div class="open-deck">
                <a :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener"><font-awesome-icon alt="View Deck in ArkhamDB" icon="external-link" /></a>
              </div>
              <div class="sync-deck">
                <a href="#" @click.prevent="sync"><font-awesome-icon icon="refresh" /></a>
              </div>
              <div class="deck-delete">
                <a href="#delete" @click.prevent="deleting = true"><font-awesome-icon icon="trash" /></a>
              </div>
            </div>
          </div>
        </div>
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
  height: calc(100% - 40px);
}

.results {
  flex: 1;
  overflow-y: auto;
}
.card {
  width: calc(100% - 20px);
  margin: 10px;
  border-radius: 10px;
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
  border-top: 1px solid #999;
  padding: 2px 0;
}

tr:nth-child(even) {
  background-color: #f2f2f2;
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
  background-color: #777;
  color: white;
}

thead tr th {
  background-color: #BBB;

  &:nth-child(1) {
    border-top-left-radius: 10px;
  }

  &:last-child {
    border-top-right-radius: 10px;
  }
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
    color: #660000;
    &:hover {
      color: #990000;
    }
  }
}

.portrait--decklist {
  width: 300px;
  margin-right: 10px;
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
  margin-bottom: 0;
  padding: 20px;
  a {
    color: #365488;
    font-weight: bolder;
  }

  display: flex;

  width: calc(100% - 40px);
  box-sizing: border-box;
  border-bottom-radius: 10px;
  background-color: rgba(0,0,0,0.3);

  color: white;

  input {
    margin-right: 10px;
  }

  button {
    padding: 5px;
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
