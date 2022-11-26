<script lang="ts" setup>
import { watchEffect, ref, computed, inject } from 'vue';
import { fetchCards } from '@/arkham/api';
import * as Arkham from '@/arkham/types/CardDef';

const allCards = ref<Arkham.CardDef[]>([])
const ready = ref(false)
const baseUrl = inject('baseUrl')
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

const image = (card: Arkham.CardDef) => `${baseUrl}/img/arkham/cards/${card.art}.jpg`
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
    switch(s) {
      case "SkillWillpower": return "willpower"
      case "SkillIntellect": return "intellect"
      case "SkillCombat": return "combat"
      case "SkillAgility": return "agility"
      case "SkillWild": return "wild"
      default: return "unknown"
    }
  })
}

const cycles = [
  { name: "Core", cycle: 1, code: "core" },
  { name: "The Dunwich Legacy", cycle: 2, code: "dwl" },
  { name: "The Path to Carcosa", cycle: 3, code: "ptc" },
  { name: "The Forgotten Age", cycle: 4, code: "tfa" },
  { name: "The Circle Undone", cycle: 5, code: "tcu" },
  { name: "The Dream-Eaters", cycle: 6, code: "tde" },
  { name: "The Innsmouth Conspiracy", cycle: 7, code: "tic" },
  { name: "The Edge of the Earth", cycle: 8, code: "eoe" },
  { name: "The Scarlet Keys", cycle: 9, code: "tsk" },
  { name: "Return to...", cycle: 50, code: "return" },
  { name: "Investigator Starter Decks", cycle: 60, code: "investigator" },
  { name: "Side Stories", cycle: 70, code: "side_stories" },
  { name: "Promotional", cycle: 80, code: "promotional" },
  { name: "Parallel", cycle: 90, code: "parallel" },
]

const sets = [
  { name: "Core Set", min: 1001, max: 1182, playerCards: 103, code: "core", cycle: 1 },
  { name: "Revised Core Set", min: 1501, max: 1695, playerCards: 116, code: "rcode", cycle: 1 },

  { name: "The Dunwich Legacy", min: 2001, max: 2104, playerCards: 39, code: "dwl", cycle: 2 },
  { name: "The Miskatonic Museum", min: 2105, max: 2146, playerCards: 13, code: "tmm", cycle: 2 },
  { name: "The Essex County Express", min: 2147, max: 2183, playerCards: 12, code: "tece", cycle: 2 },
  { name: "Blood on the Altar", min: 2184, max: 2224, playerCards: 11, code: "bota", cycle: 2 },
  { name: "Undimensioned and Unseen", min: 2225, max: 2259, playerCards: 11, code: "uau", cycle: 2 },
  { name: "Where Doom Awaits", min: 2260, max: 2298, playerCards: 14, code: "wda", cycle: 2 },
  { name: "Lost in Time and Space", min: 2299, max: 2333, playerCards: 12, code: "litas", cycle: 2 },

  { name: "The Path to Carcosa", min: 3001, max: 3105, playerCards: 42, encounterDuplicates: 5, code: "ptc", cycle: 3 },
  { name: "Echoes of the Past", min: 3106, max: 3146, playerCards: 14, code: "eotp", cycle: 3 },
  { name: "The Unspeakable Oath", min: 3147, max: 3188, playerCards: 12, code: "tuo", cycle: 3 },
  { name: "A Phantom of Truth", min: 3189, max: 3227, playerCards: 11, code: "apot", cycle: 3 },
  { name: "The Pallid Mask", min: 3228, max: 3262, playerCards: 12, code: "tpm", cycle: 3 },
  { name: "Black Stars Rise", min: 3263, max: 3303, playerCards: 11, code: "bsr", cycle: 3 },
  { name: "Dim Carcosa", min: 3304, max: 3342, encounterDuplicates: 6, playerCards: 12, code: "dca", cycle: 3 },

  { name: "The Forgotten Age", min: 4001, max: 4102, playerCards: 42, code: "tfa", cycle: 4 },
  { name: "Threads of Fate", min: 4103, max: 4148, playerCards: 10, code: "tof", cycle: 4 },
  { name: "The Boundary Beyond", min: 4149, max: 4191, playerCards: 12, code: "tbb", cycle: 4 },
  { name: "Heart of the Elders", min: 4192, max: 4228, playerCards: 13, code: "hote", cycle: 4 },
  { name: "The City of Archives", min: 4229, max: 4264, playerCards: 8, code: "tcoa", cycle: 4 },
  { name: "The Depths of Yoth", min: 4265, max: 4303, playerCards: 12, code: "tdoy", cycle: 4 },
  { name: "Shattered Aeons", min: 4304, max: 4347, playerCards: 12, code: "sha", cycle: 4 },

  { name: "The Circle Undone", min: 5001, max: 5108, playerCards: 42, code: "tcu", cycle: 5 },
  { name: "The Secret Name", min: 5109, max: 5150, playerCards: 11, code: "tsn", cycle: 5 },
  { name: "The Wages of Sin", min: 5151, max: 5185, playerCards: 10, code: "wos", cycle: 5 },
  { name: "For the Greater Good", min: 5186, max: 5228, playerCards: 11, code: "fgg", cycle: 5 },
  { name: "Union and Disillusion", min: 5229, max: 5272, playerCards: 9, code: "uad", cycle: 5 },
  { name: "In the Clutches of Chaos", min: 5273, max: 5312, playerCards: 11, code: "icc", cycle: 5 },
  { name: "Before the Black Throne", min: 5313, max: 5346, playerCards: 12, code: "bbt", cycle: 5 },

  { name: "The Dream-Eaters", min: 6001, max: 6109, playerCards: 38, code: "tde", cycle: 6 },
  { name: "The Search for Kadath", min: 6110, max: 6154, playerCards: 9, code: "sfk", cycle: 6 },
  { name: "A Thousand Shapes of Horror", min: 6155, max: 6194, playerCards: 13, code: "tsh", cycle: 6 },
  { name: "Dark Side of the Moon", min: 6195, max: 6233, playerCards: 11, code: "dsm", cycle: 6 },
  { name: "Point of No Return", min: 6234, max: 6275, playerCards: 13, code: "pnr", cycle: 6 },
  { name: "Where the Gods Dwell", min: 6276, max: 6322, playerCards: 10, code: "wgd", cycle: 6 },
  { name: "Weaver of the Cosmos", min: 6323, max: 6354, playerCards: 10, code: "woc", cycle: 6 },

  { name: "The Innsmouth Conspiracy", min: 7001, max: 7107, playerCards: 40, code: "tic", cycle: 7 },
  { name: "In Too Deep", min: 7108, max: 7151, playerCards: 15, code: "itd", cycle: 7 },
  { name: "Devil Reef", min: 7152, max: 7188, playerCards: 11, code: "def", cycle: 7 },
  { name: "Horror in High Gear", min: 7189, max: 7219, playerCards: 9, code: "hhg", cycle: 7 },
  { name: "A Light in the Fog", min: 7220, max: 7260, playerCards: 11, code: "lif", cycle: 7 },
  { name: "The Lair of Dagon", min: 7261, max: 7300, playerCards: 13, code: "lod", cycle: 7 },
  { name: "Into the Maelstrom", min: 7301, max: 7338, playerCards: 10, code: "itm", cycle: 7 },

  { name: "Edge of the Earth Investigator Expansion", min: 8001, max: 8133, playerCards: 133, code: "eoep", cycle: 8 },
  { name: "Edge of the Earth Campaign Expansion", min: 8501, max: 8738, playerCards: 0, code: "eoec", cycle: 8 },

  { name: "The Scarlet Keys Investigator Expansion", min: 9001, max: 9128, playerCards: 128, code: "tskp", cycle: 9 },
  { name: "The Scarlet Keys Campaign Expansion", min: 9501, max: 9999, playerCards: 0, code: "tskc", cycle: 9 }, // I don't yet know the final card number


  { name: "Return to Night of the Zealot", min: 50001, max: 50046, playerCards: 10, code: "rtnotz", cycle: 50 },
  { name: "Return to The Dunwich Legacy", min: 51001, max: 51072, playerCards: 11, code: "rtdwl", cycle: 50 },
  { name: "Return to The Path to Carcosa", min: 52001, max: 52078, playerCards: 13, code: "rtptc", cycle: 50 },
  { name: "Return to The Forgotten Age", min: 53001, max: 53080, playerCards: 15, code: "rttfa", cycle: 50 },
  { name: "Return to The Circle Undone", min: 54001, max: 54075, playerCards: 15, code: "rttcu", cycle: 50 },

  { name: "Nathaniel Cho", min: 60101, max: 60132, playerCards: 32, code: "nat", cycle: 60 },
  { name: "Harvey Walters", min: 60201, max: 60233, playerCards: 33, code: "har", cycle: 60 },
  { name: "Winifred Habbamock", min: 60301, max: 60332, playerCards: 32, code: "win", cycle: 60 },
  { name: "Jacqueline Fine", min: 60401, max: 60432, playerCards: 32, code: "jac", cycle: 60 },
  { name: "Stella Clark", min: 60501, max: 60531, playerCards: 31, code: "ste", cycle: 60 },

  { name: "Curse of the Rougarou", min: 81001, max: 81036, playerCards: 0, code: "cotr", cycle: 70 },
  { name: "Carnevale of Horrors", min: 82001, max: 82037, playerCards: 0, code: "coh", cycle: 70 },
  { name: "The Labyrinths of Lunacy", min: 70001, max: 70061, playerCards: 0, code: "lol", cycle: 70 },
  { name: "Guardians of the Abyss", min: 83001, max: 83058, playerCards: 0, code: "guardians", cycle: 70 },
  { name: "Murder at the Excelsior Hotel", min: 84001, max: 84052, playerCards: 0, code: "hotel", cycle: 70 },
  { name: "The Blob That Ate Everything", min: 85001, max: 85053, playerCards: 0, code: "blob", cycle: 70 },
  { name: "War of the Outer Gods", min: 86001, max: 86055, playerCards: 0, code: "wog", cycle: 70 },
  { name: "Machinations Through Time", min: 87001, max: 87999, playerCards: 0, code: "mtt", cycle: 70 }, // TODO: actual end

  { name: "Hour of the Huntress", min: 98001, max: 98003, playerCards: 3, code: "hoth", cycle: 80 },
  { name: "The Dirge of Reason", min: 98004, max: 98006, playerCards: 3, code: "tdor", cycle: 80 },
  { name: "Ire of the Void", min: 98007, max: 98009, playerCards: 3, code: "iotv", cycle: 80 },
  { name: "To Fight the Black Wind", min: 98010, max: 98012, playerCards: 3, code: "tftbw", cycle: 80 },
  { name: "The Deep Gate", min: 98013, max: 98015, playerCards: 3, code: "tdg", cycle: 80 },
  { name: "Blood of Baalshandor", min: 98016, max: 98018, playerCards: 3, code: "bob", cycle: 80 },
  { name: "Dark Revelations", min: 98019, max: 98021, playerCards: 3, code: "dre", cycle: 80 },
  { name: "Promo", min: 99001, max: 99003, playerCards: 3, code: "promo", cycle: 80 },

  { name: "Read or Die", min: 90001, max: 90007, playerCards: 3, code: "rod", cycle: 90 },
  { name: "All or Nothing", min: 90008, max: 90016, playerCards: 3, code: "aon", cycle: 90 },
  { name: "Bad Blood", min: 90017, max: 90023, playerCards: 3, code: "bad", cycle: 90 },
  { name: "By the Book", min: 90024, max: 90036, playerCards: 8, code: "btb", cycle: 90 },
  { name: "Red Tide Rising", min: 90037, max: 90045, playerCards: 4, code: "rtr", cycle: 90 },

]


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
        <img class="card" v-for="card in cards" :key="card.art" :src="image(card)" />
      </div>
      <table class="list" v-if="view == View.List">
        <thead>
          <tr><th>Name</th><th>Class</th><th>Cost</th><th>Type</th><th>Icons</th><th>Traits</th><th>Set</th></tr>
        </thead>
        <tbody>
          <tr v-for="card in cards" :key="card.art">
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

<style scoped lang="scss">
.container {
  display: flex;
  height: 100%;
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
  grid-template-columns: repeat(auto-fill, calc(1 / 10 * 100%));
  padding: 10px;
  box-sizing: border-box;
}

@media (max-width: 2000px) {
  .cards {
    grid-template-columns: repeat(auto-fill, calc(1 / 8 * 100%));
  }
}

@media (max-width: 1700px) {
  .cards {
    grid-template-columns: repeat(auto-fill, calc(1 / 6 * 100%));
  }
}

@media (max-width: 1400px) {
  .cards {
    grid-template-columns: repeat(auto-fill, calc(1 / 5 * 100%));
  }
}


@media (max-width: 1100px) {
  .cards {
    grid-template-columns: repeat(auto-fill, calc(1 / 3 * 100%));
  }
}

@media (max-width: 900px) {
  .cards {
    grid-template-columns: repeat(auto-fill, calc(1 / 2 * 100%));
  }
}

@media (max-width: 700px) {
  .cards {
    grid-template-columns: repeat(auto-fill, calc(100%));
  }
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
