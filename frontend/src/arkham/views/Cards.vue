<script lang="ts" setup>
import { watchEffect, ref, computed } from 'vue';
import { fetchCards } from '@/arkham/api';
import { imgsrc } from '@/arkham/helpers';
import * as Arkham from '@/arkham/types/CardDef';

import sets from '@/arkham/data/sets.json'
import cycles from '@/arkham/data/cycles.json'

const allCards = ref<Arkham.CardDef[] | null>(null)
const includeEncounter = ref(false)

interface Filter {
  cardType: string | null
  text: string[]
  level: number | null
  cycle: number | null
  set: string | null
  classes: string[]
  traits: string[]
}

interface CardSet {
  name: string
  min: number
  max: number
  playerCards: number
  code: string
  cycle: number
  encounterDuplicates?: number
}

interface CardCycle {
  name: string
  cycle: number
  code: string
}

enum View {
  Image = "IMAGE",
  List = "LIST",
}

watchEffect(async () => {
  await fetchCards(includeEncounter.value).then(async (response) => {
    allCards.value = response.sort((a, b) => {
      if (a.art < b.art) return -1
      if (a.art > b.art) return 1
      return 0
    })
  })
})

const cycleCount = (cycle: CardCycle) => {
  if (!allCards.value) return 0
  const cycleSets = sets.filter((s) => s.cycle == cycle.cycle)
  return allCards.value.filter((c) => {
    const cSet = cardSet(c)
    return cSet ? cycleSets.includes(cSet) : false
  }).length
}

const cycleCountText = (cycle: CardCycle) => {
  if (!allCards.value) return 0
  const implementedCount = cycleCount(cycle)
  const cycleSets = sets.filter((s) => s.cycle == cycle.cycle)
  const total = cycleSets.reduce((acc, set) => acc + (includeEncounter.value ? set.max - set.min + 1 + (set.encounterDuplicates ? set.encounterDuplicates : 0) : set.playerCards), 0)

  if (implementedCount == total) {
    return ""
  }

  return ` (${implementedCount}/${total})`
}

const setCount = (set: CardSet) => {
  if (!allCards.value) return 0
  return allCards.value.filter((c) => cardSet(c) == set).length
}

const setCountText = (set: CardSet) => {
  const implementedCount = setCount(set)
  const total = includeEncounter.value ? set.max - set.min + 1 + (set.encounterDuplicates ? set.encounterDuplicates : 0) : set.playerCards

  if (implementedCount == total) {
    return ""
  }

  return ` (${implementedCount}/${total})`
}

const image = (card: Arkham.CardDef) => imgsrc(`cards/${card.art}.jpg`)
const view = ref(View.List)

const query = ref("e:core")
const filter = ref<Filter>({ cardType: null, text: [], level: null, cycle: null, set: "core", classes: [], traits: [] })

const cards = computed(() => {
  if (!allCards.value) return []
  let all = allCards.value

  const { classes, traits, cycle, set, text, level, cardType: cardTypeText } = filter.value

  if (cycle) {
    const cycleSets = sets.filter((s) => s.cycle == cycle)
    all = all.filter((c) => {
      const cSet = cardSet(c)
      return cSet ? cycleSets.includes(cSet) : false
    })
  }

  if (set) {
    all = all.filter((c) => cardSet(c)?.code == set)
  }

  if (classes.length > 0) {
    all = all.filter((c) => c.classSymbols.some((cs) => classes.includes(cs.toLowerCase())))
  }

  if (traits.length > 0) {
    all = all.filter((c) => c.cardTraits.some((cs) => traits.includes(cs.toLowerCase())))
  }

  if (text.length > 0) {
    all = all.filter((c) => {
      const cardNameMatches = text.some((t) => cardName(c).toLowerCase().includes(t.toLowerCase()))
      const cardCodeMatches = text.some((t) => c.cardCode == `c${t.toLowerCase()}`)
      return cardNameMatches || cardCodeMatches
    })
  }

  if (level) {
    all = all.filter((c) => c.level == level)
  }

  if (cardTypeText) {
    all = all.filter((c) => cardType(c).toLowerCase() === cardTypeText.toLowerCase())
  }

  return all.filter((c) => c.cardCode !== "cx05184")
})

const setFilter = () => {

  let queryString = query.value
  let cardType = null
  let level = null
  let cycle = null
  let set = null
  let classes : string[] = []
  let traits : string[] = []

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


  const matchTraits = queryString.match(/k:([^ ]*)/)

  if (matchTraits) {
    queryString = queryString.replace(/k:([^ ]*)/, '')
    traits = matchTraits[1].split('|')
  }

  filter.value = { classes, cycle, set, cardType, level, traits, text: queryString.trim() !== "" ? queryString.trim().split('|') : []}
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
  if (!card.level) return ''
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

const cycleSets = (cycle: CardCycle) => {
  return sets.filter((s) => s.cycle == cycle.cycle)
}

const setCycle = (cycle: CardCycle) => {
  query.value = `y:${cycle.cycle}`
  filter.value = { cardType: null, text: [], level: null, cycle: cycle.cycle, set: null, classes: [], traits: [] }
}

const setSet = (set: CardSet) => {
  query.value = `e:${set.code}`
  filter.value = { cardType: null, text: [], level: null, cycle: null, set: set.code, classes: [], traits: [] }
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
      <table class="box" v-if="view == View.List">
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
  max-width: unset;
  margin: 0;
}

.results {
  flex: 1;
  overflow-y: auto;
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

.willpower {
  font-size: 1.5em;
  margin: 0 2px;
  color: var(--willpower);
}

.intellect {
  font-size: 1.5em;
  margin: 0 2px;
  color: var(--intellect);
}

.combat {
  font-size: 1.5em;
  margin: 0 2px;
  color: var(--combat);
}

.agility {
  font-size: 1.5em;
  margin: 0 2px;
  color: var(--agility);
}

.wild {
  font-size: 1.5em;
  margin: 0 2px;
  color: var(--wild);
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
  color: #CECECE;
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

header {
  position: sticky;
  position: -webkit-sticky;
  width: 100%;
  top: -1px;
  background: color-mix(in srgb, var(--background) 90%, transparent);
  box-sizing: border-box;
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

#include-encounter {
  display: inline;
}

table.box {
  width: calc(100% - 40px);
  box-sizing: border-box;
  padding: 0;
  margin: 20px;
  margin-top: 0;
  border-radius: 10px;
  border-spacing: 0;
  background-color: rgba(255,255,255,0.05);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
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
  padding: 5px;
}

thead tr th {
  color: #aaa;
  background-color: rgba(0, 0, 0, 0.2);
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

tr:nth-child(even) {
  background-color: rgba(0, 0, 0, 0.1);
}
</style>
