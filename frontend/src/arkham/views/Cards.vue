<script lang="ts" setup>
import { watchEffect, ref, computed } from 'vue';
import { fetchCards } from '@/arkham/api';
import { imgsrc } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';

import sets from '@/arkham/data/sets.json'
import cycles from '@/arkham/data/cycles.json'

const allCards = ref<Arkham.CardDef[]>([])
const ready = ref(false)
const includeEncounter = ref(false)

interface Filter {
  cardType: string | null
  text: string | null
  level: number | null
  cycle: number | null
  set: string | null
  classes: string[]
}

enum View {
  Image = "IMAGE",
  List = "LIST",
}

watchEffect(() => {
  fetchCards(includeEncounter.value).then((response) => {
    allCards.value = response.sort((a, b) => {
      if (a.art < b.art) {
        return -1
      }

      if (a.art > b.art) {
        return 1
      }

      return 0
    })
    ready.value = true
  })
})

const cycleCount = (cycle) => {
  const cycleSets = sets.filter((s) => s.cycle == cycle.cycle)
  return allCards.value.filter((c) => cycleSets.includes(cardSet(c))).length
}

const cycleCountText = (cycle) => {
  const implementedCount = cycleCount(cycle)
  const cycleSets = sets.filter((s) => s.cycle == cycle.cycle)
  const total = cycleSets.reduce((acc, set) => acc + (includeEncounter.value ? set.max - set.min + 1 + (set.encounterDuplicates ? set.encounterDuplicates : 0) : set.playerCards), 0)

  if (implementedCount == total) {
    return ""
  }

  return ` (${implementedCount}/${total})`
}

const setCount = (set) => {
  return allCards.value.filter((c) => cardSet(c) == set).length
}

const setCountText = (set) => {
  const implementedCount = setCount(set)
  const total = includeEncounter.value ? set.max - set.min + 1 + (set.encounterDuplicates ? set.encounterDuplicates : 0) : set.playerCards

  if (implementedCount == total) {
    return ""
  }

  return ` (${implementedCount}/${total})`
}

const image = (card: Arkham.CardDef) => imgsrc(`cards/${card.art}.jpg`)
const view = ref(View.List)

const query = ref("")
const filter = ref<Filter>({ cardType: null, text: null, level: null, cycle: null, set: null, classes: [] })

const cards = computed(() => {
  let all = allCards.value

  const { classes, cycle, set, text, level, cardType: cardTypeText } = filter.value

  if (cycle) {
    const cycleSets = sets.filter((s) => s.cycle == cycle)
    all = all.filter((c) => cycleSets.includes(cardSet(c)))
  }

  if (set) {
    all = all.filter((c) => cardSet(c)?.code == set)
  }

  if (classes.length > 0) {
    all = all.filter((c) => c.classSymbols.some((cs) => classes.includes(cs.toLowerCase())))
  }

  if (text) {
    all = all.filter((c) => cardName(c).toLowerCase().includes(text.toLowerCase()))
  }

  if (level) {
    all = all.filter((c) => c.level == level)
  }

  if (cardTypeText) {
    all = all.filter((c) => cardType(c).toLowerCase() === cardTypeText.toLowerCase())
  }

  return all
})

const setFilter = () => {

  let queryString = query.value
  let cardType = null
  let level = null
  let cycle = null
  let set = null
  let classes = []

  // const matchCardType = queryString.match(/t:("(?:[^"\\]|\\.)*"|[^ ]*)/)
  const matchCardType = queryString.match(/t:([^ ]*)/)

  if (matchCardType) {
    queryString = queryString.replace(/t:([^ ]*)/, '')
    cardType = matchCardType[1]
  }

  const matchLevel = queryString.match(/p:([1-9][0-9]*)/)

  if (matchLevel) {
    queryString = queryString.replace(/p:([1-9][0-9]*)/, '')
    level = parseInt(matchLevel[1])
  }

  const matchClasses = queryString.match(/f:([^ ]*)/)

  if (matchClasses) {
    queryString = queryString.replace(/f:([^ ]*)/, '')
    classes = matchClasses[1].split('|')
  }

  const matchCycle = queryString.match(/y:([1-9][0-9]*)/)

  if (matchCycle) {
    queryString = queryString.replace(/y:([1-9][0-9]*)/, '')
    cycle = parseInt(matchCycle[1])
  }

  const matchSet = queryString.match(/e:([^ ]*)/)

  if (matchSet) {
    queryString = queryString.replace(/e:([^ ]*)/, '')
    set = matchSet[1]
  }

  filter.value = { classes, cycle, set, cardType, level, text: queryString.trim() !== "" ? queryString.trim() : null}
}

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

const cycleSets = (cycle) => {
  return sets.filter((s) => s.cycle == cycle.cycle)
}

const setCycle = (cycle) => {
  query.value = `y:${cycle.cycle}`
  filter.value = { cardType: null, text: null, level: null, cycle: cycle.cycle, set: null, classes: [] }
}

const setSet = (set) => {
  query.value = `e:${set.code}`
  filter.value = { cardType: null, text: null, level: null, cycle: null, set: set.code, classes: [] }
}
</script>

<template>
  <div class="container">
    <div class="cycles">
      <ol>
        <li v-for="cycle in cycles" :key="cycle.code">
          <a href="#" @click.prevent="setCycle(cycle)">{{cycle.name}}</a>{{cycleCountText(cycle)}}
          <ol>
            <li v-for="set in cycleSets(cycle)" :key="set.code">
              <a href="#" @click.prevent="setSet(set)">{{set.name}}</a>{{setCountText(set)}}
            </li>
          </ol>
        </li>
      </ol>
    </div>
    <div class="results">
      <header>
        <form @submit.prevent="setFilter">
          <input v-model="query" />
          <button type="submit"><font-awesome-icon icon="search" /></button>
        </form>
        <button @click.prevent="view = View.List" :class="{ pressed: view == View.List }"><font-awesome-icon icon="list" /></button>
        <button @click.prevent="view = View.Image" :class="{ pressed: view == View.Image }"><font-awesome-icon icon="image" /></button>
        <div>
          <label for="include-encounter">
            <input type="checkbox" v-model="includeEncounter" id="include-encounter" />
            Include Encounter
          </label>
        </div>
      </header>
      <div class="cards" v-if="view == View.Image">
        <a v-for="card in cards" :key="card.art" target="_blank" :href="`https://arkhamdb.com/card/${card.art}`">
          <img class="card" :src="image(card)" />
        </a>
      </div>
      <table class="list" v-if="view == View.List">
        <thead>
          <tr><th>Name</th><th>Class</th><th>Cost</th><th>Type</th><th>Icons</th><th>Traits</th><th>Set</th></tr>
        </thead>
        <tbody>
          <tr v-for="card in cards" :key="card.art">
            <td><a target="_blank" :href="`https://arkhamdb.com/card/${card.art}`">{{cardName(card)}}{{levelText(card)}}</a></td>
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
  border-top: 1px solid #111;
  padding: 2px 0;
}

tr {
  color: #ccc;
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
  background-color: #777;
  color: white;
}

thead tr th {
  background-color: #111;
  color: #aaa;

  &:nth-child(1) {
    border-top-left-radius: 10px;
  }

  &:last-child {
    border-top-right-radius: 10px;
  }
}

header {
  margin-left: 20px;
  margin-right: 20px;
  background: rgba(0,0,0,0.3);
  display: flex;
  align-items: center;
  color: white;
  padding: 10px 20px 15px 20px;
  border-bottom-left-radius: 10px;
  border-bottom-right-radius: 10px;

  input {
    margin-right: 10px;
  }

  input[type=checkbox] {
    margin-left: 20px;
  }

  button {
    padding: 5px;
  }

  form {
    margin-right: 20px;
  }

  form button {
    border: 0;
    height: 100%;
    box-sizing: content-box;
    padding: 5px;
  }

  form input {
    outline: none;
    padding: 5px;
    border: 0;
    height: 100%;
    margin: 0;
    box-sizing: content-box;
  }
}
</style>
