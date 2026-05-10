<script lang="ts" setup>
import { watch, ref, computed } from 'vue';
import { fetchCards } from '@/arkham/api';
import { useRouter, useRoute, LocationQueryValue } from 'vue-router';
import * as Arkham from '@/arkham/types/CardDef';
import CardListView from '@/arkham/components/CardListView.vue';
import CardImageView from '@/arkham/components/CardImageView.vue';
import sets from '@/arkham/data/sets.json'
import cycles from '@/arkham/data/cycles.json'
import { shallowRef } from 'vue';
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'

enum View {
  Image = "IMAGE",
  List = "LIST",
}

const CHAPTER_2_CYCLES = new Set([12, 61])

const SET_FONT_CHARS: Record<string, string> = {
  // CHAPTER 1
  // Cycle 1 — Core
  core:      '\uEA9F', rcode:     '\uEA9F',
  // Cycle 2 — The Dunwich Legacy
  dwl:       '\u0049', tmm:       '\uEA6E', tece:      '\uEA6F',
  bota:      '\uEA70', uau:       '\uEA71', wda:       '\uEA72', litas:     '\uEA73',
  // Cycle 3 — The Path to Carcosa
  ptc:       '\u0047', eotp:      '\uEA4D', tuo:       '\uEA4E',
  apot:      '\uEA4F', tpm:       '\uEA50', bsr:       '\uEA51', dca:       '\uEA54',
  // Cycle 4 — The Forgotten Age
  tfa:       '\u0045', tof:       '\uE9FA', tbb:       '\uE9FB',
  hote:      '\uE9FE', tcoa:      '\uE9FF', tdoy:      '\uEA00', sha:       '\uEA01',
  // Cycle 5 — The Circle Undone
  tcu:       '\u0043', tsn:       '\uE9D6', wos:       '\uE9D7',
  fgg:       '\uE9D8', uad:       '\uE9D9', icc:       '\uE9DA', bbt:       '\uE9DB',
  // Cycle 6 — The Dream-Eaters
  tde:       '\u0042', sfk:       '\uE9A4', tsh:       '\uE9A5',
  dsm:       '\uE9A6', pnr:       '\uE9A7', wgd:       '\uE9A8', woc:       '\uE9A9',
  // Cycle 7 — The Innsmouth Conspiracy
  tic:       '\u0041', itd:       '\uE99D', def:       '\uE99E',
  hhg:       '\uE99F', lif:       '\uE9A0', lod:       '\uE9A1', itm:       '\uE9A2',
  // Cycle 8 — The Edge of the Earth
  eoep:      '\uE977', eoec:      '\uE978',
  // Cycle 9 — The Scarlet Keys
  tskp:      '\uE937', tskc:      '\uE938',
  // Cycle 10 — The Feast of Hemloch Vale
  fhvp:      '\uE9B9', fhvc:      '\uE9BA',
  // Cycle 11 — The Drowned City
  tdcp:      '\uE936', tdcc:      '\uE92E',
  // Cycle 12 — Core 2026
  core2026:  '\uE900',
  // Cycle 50 — Return to...
  rtnotz:    '\u0058', rtdwl:     '\u004A', rtptc:     '\u0048',
  rttfa:     '\u0046', rttcu:     '\u0044',
  // Cycle 60 — Investigator Starter Decks
  nat:       '\u004B', har:       '\u004D', win:       '\u004F',
  jac:       '\u004E', ste:       '\u0050',
  // Cycle 61 — Investigator Starter Decks (Chapter 2)
  tom:       '\uE918',
  car:       '\uE919',
  and:       '\uE91B',
  mar:       '\uE91A',
  mig:       '\uE91C',
  // Cycle 70 — Side Stories
  cotr:      '\u0051', coh:       '\uEA24', lol:       '\u0053',
  guardians: '\u0054', hotel:     '\u0055', blob:      '\u0056',
  wog:       '\uEA3B', mtt:       '\uEA47', faf:       '\uEA42',
  blbe:      '\uEA4A', tmg:       '\uEA1F', ff:        '\uEA8B',
  // Cycle 80 — Promotional
  hoth:      '\uEA20', tdor:      '\uEA20', iotv:      '\uEA20', tftbw:     '\uEA20',
  tdg:       '\uEA20', bob:       '\uEA20', dre:       '\uEA20', promo:     '\uEA20',
  // Cycle 90 — Parallel
  rod:       '\uEA36', aon:       '\uEA37', bad:       '\uEA38', btb:       '\uEA39',
  rtr:       '\uEA3A', otr:       '\uEA1E', ltr:       '\uEA48', ptr:       '\uEA1E',
  rop:       '\uEA49', hfa:       '\uEA1E', aof:       '\uEA1E', pap:       '\uEA1E',
  ee:        '\uEA8E',
}

const toView = (view: string | LocationQueryValue[]): View => {
  if (view === "IMAGE") return View.Image
  return View.List
}

const fromView = (view: View): string => {
  if (view === View.Image) return "IMAGE"
  return "LIST"
}

const router = useRouter()
const route = useRoute()
const queryText = route.query.q ? route.query.q.toString() : "e:core"
const allCards = shallowRef<Arkham.CardDef[] | null>(null)
const query = ref<string>(queryText)
const view = ref(route.query.view? toView(route.query.view) : View.List)
const activeChapter = ref<number>(route.query.chapter ? parseInt(route.query.chapter.toString()) : 1)

const includeEncounter = computed(() => route.query.includeEncounter === "true")
const store = useDbCardStore()

const CACHE_KEY_PREFIX = 'arkham_cards_cache_'
const CACHE_VERSION = 'v1'
const CACHE_TTL_MS = 5 * 60 * 1000 // 5 minutes

const getCachedCards = (withEncounter: boolean): Arkham.CardDef[] | null => {
  const key = `${CACHE_KEY_PREFIX}${CACHE_VERSION}_${withEncounter}`
  try {
    const cached = sessionStorage.getItem(key)
    if (cached) {
      const { cards, timestamp } = JSON.parse(cached)
      if (Date.now() - timestamp < CACHE_TTL_MS) return cards
    }
  } catch { /* ignore */ }
  return null
}

const setCachedCards = (cards: Arkham.CardDef[], withEncounter: boolean) => {
  const key = `${CACHE_KEY_PREFIX}${CACHE_VERSION}_${withEncounter}`
  try {
    sessionStorage.setItem(key, JSON.stringify({ cards, timestamp: Date.now() }))
  } catch { /* ignore quota errors */ }
}

const fetchData = async () => {
  const cached = getCachedCards(includeEncounter.value)
  if (cached) {
    allCards.value = cached
    return
  }
  fetchCards(includeEncounter.value).then(async (response) => {
    const sorted = response.sort((a, b) => {
      if (a.art < b.art) return -1
      if (a.art > b.art) return 1
      return 0
    })
    setCachedCards(sorted, includeEncounter.value)
    allCards.value = sorted
  })
}

interface Filter {
  cardTypes: string[]
  text: string[]
  level: number | null
  cycle: number | null
  set: string | null
  classes: string[]
  traits: string[]
  encounterSets: string[]
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

const filter = ref<Filter>({ cardTypes: [], text: [], level: null, cycle: null, set: "core", classes: [], traits: [], encounterSets: []})

await fetchData()

watch(() => includeEncounter.value, (newIncludeEncounter) => {
  router.push({ name: 'Cards', query: { ...route.query, includeEncounter: newIncludeEncounter ? 'true' : undefined}})
  fetchData()
})

watch(() => view.value, (newView) => {
  router.push({ name: 'Cards', query: { ...route.query, view: fromView(newView) }})
})

watch(() => activeChapter.value, (newChapter) => {
  router.push({ name: 'Cards', query: { ...route.query, chapter: newChapter === 1 ? undefined : String(newChapter) }})
})

watch(() => allCards.value, () => {
  const language = localStorage.getItem('language') || 'en'
  if (language === 'en') return
  if (!allCards.value) return

  for (const card of allCards.value) {
    const match: ArkhamDBCard | null = store.getDbCard(card.art)
    if (!match) continue

    // Name
    card.name.title = match.name
    if (match.subname) card.name.subtitle = match.subname

    // Class
    if (match.faction_name && card.classSymbols.length > 0) card.classSymbols[0] = match.faction_name
    if (match.faction2_name && card.classSymbols.length > 1) {
      card.classSymbols[1] = match.faction2_name
      if (match.faction3_name && card.classSymbols.length > 2) card.classSymbols[2] = match.faction3_name
    }

    // Type
    card.cardType = match.type_name

    // Traits
    if (match.traits) card.cardTraits = match.traits.split('.').filter(item => item != "" && item != " ")
  }
})

const chapter1Cycles = computed(() => cycles.filter((c) => !CHAPTER_2_CYCLES.has(c.cycle)))
const chapter2Cycles = computed(() => cycles.filter((c) => CHAPTER_2_CYCLES.has(c.cycle)))
const displayedCycles = computed(() => activeChapter.value === 2 ? chapter2Cycles.value : chapter1Cycles.value)

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

const cards = computed(() => {
  if (!allCards.value) return []

  const { classes, encounterSets, traits, cycle, set, text, level, cardTypes } = filter.value
  const cycleSets = cycle ? sets.filter((s) => s.cycle == cycle) : null

  return allCards.value.filter((c) => {
    if (c.cardCode === "cx05184") return false
    if (cycleSets) {
      const cSet = cardSet(c)
      if (!cSet || !cycleSets.includes(cSet)) return false
    }

    if (set) {
      let cCode = cardSet(c)?.code
      if (!cCode || cCode !== set) return false
    }

    if (classes.length > 0) {
      if (!c.classSymbols.some((cs) => classes.includes(cs.toLowerCase()))) return false
    }

    if (traits.length > 0) {
      if (!c.cardTraits.some((cs) => traits.includes(cs.toLowerCase()))) return false
    }

    if (encounterSets.length > 0) {
      const match: ArkhamDBCard | null = store.getDbCard(c.art)
      if (!match) return false
      return match.encounter_code !== undefined && encounterSets.includes(match.encounter_code)
    }

    if (text.length > 0) {
      const cardNameMatches = text.some((t) => cardName(c).toLowerCase().includes(t.toLowerCase()))
      const cardCodeMatches = text.some((t) => c.cardCode == `c${t.toLowerCase()}`)
      if (!cardNameMatches && !cardCodeMatches) return false
    }

    if (level && c.level !== level) return false

    if (cardTypes.length > 0) {
      const sanitizedCardTypes = cardTypes.map((ct) => ct.toLowerCase().trim())
      if (!sanitizedCardTypes.includes(cardType(c).toLowerCase().trim())) return false
    }

    return true
  })
})

const setFilter = () => {
  router.push({ name: 'Cards', query: { ...route.query, q: query.value }})
  let queryString = query.value
  let cardTypes: string[] = []
  let level = null
  let cycle = null
  let set = null
  let classes : string[] = []
  let traits : string[] = []
  let encounterSets : string[] = []

  const matchCardTypes = queryString.match(/t:([^ ]*)/)

  if (matchCardTypes) {
    queryString = queryString.replace(/t:([^ ]*)/, '')
    cardTypes = matchCardTypes[1].split('|')
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

  const matchEncounterSets = queryString.match(/m:([^ ]*)/)

  if (matchEncounterSets) {
    queryString = queryString.replace(/m:([^ ]*)/, '')
    encounterSets = matchEncounterSets[1].split('|').map((s) => s.toLowerCase())
  }

  filter.value = { classes, cycle, set, cardTypes, level, traits, encounterSets, text: queryString.trim() !== "" ? queryString.trim().split('|') : []}

}

const filterString = (f: Filter): string => {
  let result = f.text.join('|')

  if (f.cardTypes.length > 0) {
    result += ` t:${f.cardTypes.join('|')}`
  }

  if (f.level) {
    result += ` p:${f.level}`
  }

  if (f.cycle) {
    result += ` y:${f.cycle}`
  }

  if (f.set) {
    result += ` e:${f.set}`
  }

  if (f.classes.length > 0) {
    result += ` f:${f.classes.join('|')}`
  }

  if (f.traits.length > 0) {
    result += ` k:${f.traits.join('|')}`
  }

  if (f.encounterSets.length > 0) {
    result += ` m:${f.encounterSets.join('|')}`
  }

  return result.trim()
}

setFilter()

const cardName = (card: Arkham.CardDef) => {
  const subtitle = card.name.subtitle === null ? "" : `: ${card.name.subtitle}`

  return `${card.name.title}${subtitle}`
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

const cardSet = (card: Arkham.CardDef) => {
  const cardCode = parseInt(card.art)
  return sets.find((s) => cardCode >= s.min && cardCode <= s.max)
}

const cycleSets = (cycle: CardCycle) => {
  return sets.filter((s) => s.cycle == cycle.cycle)
}

const CYCLE_ICON_OVERRIDES: Record<number, string> = {
  50: 'core',  // Return to...
  60: 'core',  // Investigator Starter Decks
  61: 'core',  // Investigator Starter Decks (Chapter 2)
  70: 'core',  // Side Stories
  90: 'otr',   // Parallel — On the Road Again
}

const cycleIconCode = (cycle: CardCycle): string => {
  if (CYCLE_ICON_OVERRIDES[cycle.cycle]) return CYCLE_ICON_OVERRIDES[cycle.cycle]
  return cycleSets(cycle)[0]?.code ?? ''
}

const setCycle = (cycle: CardCycle) => {
  query.value = filterString({...filter.value, set: null, cycle: cycle.cycle})
  setFilter()
}

const setSet = (set: CardSet) => {
  query.value = filterString({...filter.value, cycle: null, set: set.code})
  setFilter()
}

const toggleIncludeEncounter = () => {
  const includeEncounter = route.query.includeEncounter === 'true'
  router.push({ name: 'Cards', query: { ...route.query, includeEncounter: !includeEncounter ? 'true' : undefined }})
}

const showSidebar = ref(false)
</script>

<template>
  <div class="container">
    <div class="sidebar-overlay" :class="{ visible: showSidebar }" @click="showSidebar = false"></div>
    <div class="sidebar" :class="{ open: showSidebar }">
      <button class="sidebar-close" @click="showSidebar = false"><font-awesome-icon icon="times" /></button>
      <div class="chapter-tabs">
        <button
          :class="['chapter-tab', { active: activeChapter === 1 }]"
          @click="activeChapter = 1"
        >Chapter 1</button>
        <button
          :class="['chapter-tab', { active: activeChapter === 2 }]"
          @click="activeChapter = 2"
        >Chapter 2</button>
      </div>
      <nav class="cycles">
        <ol>
          <li v-for="cycle in displayedCycles" :key="cycle.code">
            <div class="nav-row">
              <i v-if="SET_FONT_CHARS[cycleIconCode(cycle)]" class="set-icon-font">{{ SET_FONT_CHARS[cycleIconCode(cycle)] }}</i>
              <img v-else-if="cycleIconCode(cycle)" class="set-icon" :src="`/img/arkham/encounter-sets/${cycleIconCode(cycle)}.png`" :alt="cycle.name" />
              <a href="#" @click.prevent="setCycle(cycle)">{{cycle.name}}</a>
              <span class="count">{{cycleCountText(cycle)}}</span>
            </div>
            <ol>
              <li v-for="set in cycleSets(cycle)" :key="set.code">
                <div class="nav-row nav-row--sub">
                  <i v-if="SET_FONT_CHARS[set.code]" class="set-icon-font">{{ SET_FONT_CHARS[set.code] }}</i>
                  <img v-else class="set-icon" :src="`/img/arkham/encounter-sets/${set.code}.png`" :alt="set.name" />
                  <a href="#" @click.prevent="setSet(set)">{{set.name}}</a>
                  <span class="count">{{setCountText(set)}}</span>
                </div>
              </li>
            </ol>
          </li>
        </ol>
      </nav>
    </div>
    <div class="results">
      <header>
        <button class="sidebar-toggle" @click="showSidebar = !showSidebar" :title="$t('cardsView.browseSets')">
          <font-awesome-icon class="toggle-arrow" icon="chevron-right" />
          <font-awesome-icon icon="book" />
        </button>
        <form @submit.prevent="setFilter">
          <input v-model="query" :placeholder="$t('cardsView.searchCards')" />
          <button type="submit"><font-awesome-icon icon="search" /></button>
        </form>
        <div class="view-controls">
          <button @click.prevent="view = View.List" :class="{ active: view == View.List }" :title="$t('cardsView.listView')"><font-awesome-icon icon="list" /></button>
          <button @click.prevent="view = View.Image" :class="{ active: view == View.Image }" :title="$t('cardsView.imageView')"><font-awesome-icon icon="image" /></button>
        </div>
        <label class="encounter-toggle">
          <input type="checkbox" @click="toggleIncludeEncounter" :checked="includeEncounter" id="include-encounter" />
          <span>{{ $t('cardsView.includeEncounter') }}</span>
        </label>
      </header>
      <CardImageView v-if="view == View.Image" :cards="cards" />
      <CardListView v-if="view == View.List" :cards="cards" />
    </div>
  </div>
</template>

<style scoped>
.container {
  display: flex;
  height: calc(100vh - var(--nav-height));
  max-width: unset;
  margin: 0;
  overflow: hidden;
  @media (max-width: 768px) {
    flex-direction: column;
  }
}

/* ── Sidebar ────────────────────────────────────────────── */

.sidebar {
  display: flex;
  flex-direction: column;
  width: clamp(200px, 18vw, 320px);
  border-right: 1px solid rgba(255,255,255,0.08);
  overflow: hidden;
  @media (max-width: 768px) {
    position: fixed;
    left: 0;
    top: 0;
    bottom: 0;
    width: 280px;
    max-height: unset;
    border-right: 1px solid rgba(255,255,255,0.12);
    background: var(--background);
    z-index: 50;
    transform: translateX(-100%);
    transition: transform 0.25s ease;
    overflow-y: auto;
    &.open { transform: translateX(0); }
  }
}

.sidebar-overlay {
  display: none;
  @media (max-width: 768px) {
    &.visible {
      display: block;
      position: fixed;
      inset: 0;
      background: rgba(0, 0, 0, 0.6);
      z-index: 49;
    }
  }
}

.sidebar-close {
  display: none;
  @media (max-width: 768px) {
    display: flex;
    align-self: flex-end;
    margin: 8px 8px 0 auto;
    background: transparent;
    border: none;
    color: #777;
    cursor: pointer;
    padding: 6px;
    font-size: 1.1em;
    flex-shrink: 0;
    &:hover { color: #ccc; }
  }
}

.sidebar-toggle {
  display: none;
  @media (max-width: 768px) {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 3px;
    height: 32px;
    padding: 0 8px;
    background: rgba(255,255,255,0.08);
    border: 1px solid rgba(255,255,255,0.15);
    border-radius: 6px;
    color: #aaa;
    cursor: pointer;
    flex-shrink: 0;
    &:hover { background: rgba(255,255,255,0.14); color: #eee; }
  }
}

.toggle-arrow {
  display: none;
  @media (max-width: 768px) {
    display: inline-block;
    transform: rotate(180deg);
    font-size: 0.65em;
    opacity: 0.7;
  }
}

.chapter-tabs {
  display: flex;
  border-bottom: 1px solid rgba(255,255,255,0.08);
  flex-shrink: 0;
}

.chapter-tab {
  flex: 1;
  padding: 10px 6px;
  font-size: 0.8rem;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: #888;
  background: transparent;
  border: none;
  border-bottom: 2px solid transparent;
  cursor: pointer;
  transition: color 0.15s, border-color 0.15s;

  &:hover {
    color: #ccc;
  }

  &.active {
    color: var(--spooky-green);
    border-bottom-color: var(--spooky-green);
  }
}

.cycles {
  flex: 1;
  overflow-y: auto;
  padding: 8px 0 16px;

  ol {
    list-style: none;
    margin: 0;
    padding: 0;
  }

  > ol > li + li {
    margin-top: 5px;
    border-top: 1px solid rgba(255,255,255,0.06);
  }
}

.nav-row {
  display: flex;
  align-items: center;
  overflow: hidden;
  padding: 0 10px 0 14px;

  a {
    flex: 1;
    min-width: 0;
    padding: 5px 4px 5px 0;
    font-size: 0.82rem;
    font-weight: 600;
    color: #ccc;
    text-decoration: none;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    transition: color 0.12s;

    &:hover { color: var(--spooky-green); }
  }

  .count {
    flex-shrink: 0;
    font-size: 0.72rem;
    color: #555;
    white-space: nowrap;
  }
}

.set-icon-font {
  display: inline-block;
  text-align: center;
  font-family: "ArkhamEncounters";
  font-style: normal;
  font-size: 14px;
  line-height: 1;
  width: 16px;
  flex-shrink: 0;
  margin-right: 4px;
  color: #ccc;
}

.set-icon {
  width: 16px;
  height: 16px;
  flex-shrink: 0;
  object-fit: contain;
  margin-right: 4px;
  filter: brightness(0) invert(0.8);
}

.nav-row--sub {
  padding-left: 26px;

  a {
    padding-top: 3px;
    padding-bottom: 3px;
    font-size: 0.78rem;
    font-weight: 400;
    color: #999;
  }
}

/* ── Results panel ──────────────────────────────────────── */

.results {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

header {
  display: flex;
  align-items: center;
  gap: 10px;
  flex-shrink: 0;
  padding: 10px 16px;
  background: color-mix(in srgb, var(--background) 92%, transparent);
  border-bottom: 1px solid rgba(255,255,255,0.07);
  backdrop-filter: blur(6px);
  z-index: 1;

  form {
    display: flex;
    align-items: center;
    background: rgba(255,255,255,0.06);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 6px;
    overflow: hidden;
    flex: 1;
    max-width: 360px;

    input {
      flex: 1;
      background: transparent;
      border: none;
      outline: none;
      padding: 6px 10px;
      color: #ddd;
      font-size: 0.88rem;

      &::placeholder { color: #555; }
    }

    button {
      background: transparent;
      border: none;
      padding: 6px 10px;
      color: #777;
      cursor: pointer;
      transition: color 0.12s;

      &:hover { color: var(--spooky-green); }
    }
  }
}

.view-controls {
  display: flex;
  gap: 2px;
  background: rgba(255,255,255,0.05);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 6px;
  padding: 2px;

  button {
    background: transparent;
    border: none;
    border-radius: 4px;
    padding: 5px 9px;
    color: #777;
    cursor: pointer;
    transition: background 0.12s, color 0.12s;

    &:hover { color: #ccc; }
    &.active {
      background: rgba(255,255,255,0.12);
      color: #eee;
    }
  }
}

.encounter-toggle {
  display: flex;
  align-items: center;
  gap: 6px;
  color: #999;
  font-size: 0.82rem;
  cursor: pointer;
  white-space: nowrap;
  user-select: none;

  input[type=checkbox] {
    accent-color: var(--spooky-green);
    cursor: pointer;
  }
}

</style>
