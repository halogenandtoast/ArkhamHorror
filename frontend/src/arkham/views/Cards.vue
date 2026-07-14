<script lang="ts" setup>
import { watch, ref, computed } from 'vue';
import { useI18n } from 'vue-i18n';
import { fetchCards, fetchHomebrewCards, type CardPoolMode } from '@/arkham/api';
import { useRouter, useRoute, LocationQueryValue } from 'vue-router';
import * as Arkham from '@/arkham/types/CardDef';
import CardListView from '@/arkham/components/CardListView.vue';
import CardImageView from '@/arkham/components/CardImageView.vue';
import sets from '@/arkham/data/sets.json'
import cycles from '@/arkham/data/cycles.json'
import { shallowRef } from 'vue';
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'
import { isDevBuild } from '@/arkham/displayRules'
import { homebrewCampaigns } from '@/arkham/homebrewData'
import { imgsrc } from '@/arkham/helpers'

const { t } = useI18n()

enum View {
  Image = "IMAGE",
  List = "LIST",
}

const CHAPTER_2_CYCLES = new Set([12, 61])
const HOMEBREW_CYCLE = -1
const dev = isDevBuild()

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

const cardPoolMode = computed<CardPoolMode>(() => {
  const cardPool = route.query.cardPool?.toString()
  if (cardPool === 'campaign' || cardPool === 'both') return cardPool
  return route.query.includeEncounter === 'true' ? 'both' : 'player'
})
const store = useDbCardStore()

const CACHE_KEY_PREFIX = 'arkham_cards_cache_'
const CACHE_VERSION = 'v3'
const CACHE_TTL_MS = 5 * 60 * 1000 // 5 minutes

let cachedAllCards: Arkham.CardDef[] | null = null

const sortCards = (cards: Arkham.CardDef[]) => [...cards].sort((a, b) => {
  if (a.art < b.art) return -1
  if (a.art > b.art) return 1
  return 0
})

const isCampaignCard = (card: Arkham.CardDef) => card.encounterSet != null

const cardInPool = (card: Arkham.CardDef, cardPool: CardPoolMode) => {
  if (cardPool === 'both') return true
  return cardPool === 'campaign' ? isCampaignCard(card) : !isCampaignCard(card)
}

const getCachedCards = (): Arkham.CardDef[] | null => {
  if (cachedAllCards) return cachedAllCards

  const key = `${CACHE_KEY_PREFIX}${CACHE_VERSION}_all`
  try {
    const cached = sessionStorage.getItem(key)
    if (cached) {
      const { cards, timestamp } = JSON.parse(cached)
      if (Date.now() - timestamp < CACHE_TTL_MS) {
        cachedAllCards = cards
        return cards
      }
    }
  } catch { /* ignore */ }
  return null
}

const setCachedCards = (cards: Arkham.CardDef[]) => {
  cachedAllCards = cards
  const key = `${CACHE_KEY_PREFIX}${CACHE_VERSION}_all`
  try {
    sessionStorage.setItem(key, JSON.stringify({ cards, timestamp: Date.now() }))
  } catch { /* ignore quota errors */ }
}

const fetchData = async () => {
  const cached = getCachedCards()
  if (cached) {
    allCards.value = cached
    return
  }

  const officialCards = await fetchCards('both')
  const homebrewCards = dev ? await fetchHomebrewCards() : []
  const sorted = sortCards([...officialCards, ...homebrewCards])
  setCachedCards(sorted)
  allCards.value = sorted
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
  homebrew?: boolean
  // Unused code numbers within [min, max] that don't correspond to a real card,
  // so they aren't counted toward the set total.
  missing?: string[]
}

// Total card count for an encounter set: every code in [min, max], plus any
// duplicate-back cards, minus the unused (missing) numbers in that range.
const encounterSetTotal = (set: CardSet) =>
  set.max - set.min + 1 + (set.encounterDuplicates ?? 0) - (set.missing?.length ?? 0)

interface CardCycle {
  name: string
  cycle: number
  code: string
}

interface CardSearchIndex {
  set?: CardSet
  setCode?: string
  cycle?: number
  nameLower: string
  codeLower: string
  typeLower: string
  classSymbolsLower: string[]
  traitsLower: string[]
  encounterCode?: string
}

const homebrewCycle: CardCycle = { name: 'Homebrew', cycle: HOMEBREW_CYCLE, code: 'homebrew' }
const homebrewSets: CardSet[] = dev
  ? homebrewCampaigns.map((campaign) => {
      const id = campaign.id.replace(/^:/, '')
      return {
        name: campaign.name,
        min: 0,
        max: 0,
        playerCards: 0,
        code: `homebrew-${id}`,
        cycle: HOMEBREW_CYCLE,
        homebrew: true,
      }
    })
  : []
const allCycles: CardCycle[] = dev ? [...cycles, homebrewCycle] : cycles
const allSets: CardSet[] = dev ? [...(sets as CardSet[]), ...homebrewSets] : (sets as CardSet[])

const setsByCycle = allSets.reduce<Map<number, CardSet[]>>((acc, set) => {
  const cycleSets = acc.get(set.cycle)
  if (cycleSets) cycleSets.push(set)
  else acc.set(set.cycle, [set])
  return acc
}, new Map())

const cardSetCache = new Map<string, CardSet | undefined>()

const findCardSetByArt = (art: string) => {
  const cached = cardSetCache.get(art)
  if (cached !== undefined || cardSetCache.has(art)) return cached

  const homebrewMatch = art.match(/^:([^:]+):/)
  if (homebrewMatch) {
    const set = homebrewSets.find((s) => s.code === `homebrew-${homebrewMatch[1]}`)
    cardSetCache.set(art, set)
    return set
  }

  const cardCode = parseInt(art)
  const set = allSets.find((s) => !s.homebrew && cardCode >= s.min && cardCode <= s.max)
  cardSetCache.set(art, set)
  return set
}

const filter = ref<Filter>({ cardTypes: [], text: [], level: null, cycle: null, set: "core", classes: [], traits: [], encounterSets: []})

await fetchData()

watch(() => view.value, (newView) => {
  router.push({ name: 'Cards', query: { ...route.query, view: fromView(newView) }})
})

watch(() => activeChapter.value, (newChapter) => {
  router.push({ name: 'Cards', query: { ...route.query, chapter: newChapter === 1 ? undefined : String(newChapter) }})
  if (newChapter === HOMEBREW_CYCLE) {
    query.value = filterString({ ...filter.value, cycle: null, set: 'homebrew' })
    setFilter()
  }
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

const chapter1Cycles = computed(() => allCycles.filter((c) => !CHAPTER_2_CYCLES.has(c.cycle) && c.cycle !== HOMEBREW_CYCLE))
const chapter2Cycles = computed(() => allCycles.filter((c) => CHAPTER_2_CYCLES.has(c.cycle)))
const homebrewCycles = computed(() => allCycles.filter((c) => c.cycle === HOMEBREW_CYCLE))
const displayedCycles = computed(() => {
  if (activeChapter.value === HOMEBREW_CYCLE) return homebrewCycles.value
  return activeChapter.value === 2 ? chapter2Cycles.value : chapter1Cycles.value
})

const cardSearchIndex = computed(() => {
  const index = new Map<string, CardSearchIndex>()

  for (const card of allCards.value ?? []) {
    const set = findCardSetByArt(card.art)
    const match: ArkhamDBCard | null = store.getDbCard(card.art)

    index.set(card.cardCode, {
      set,
      setCode: set?.code,
      cycle: set?.cycle,
      nameLower: cardName(card).toLowerCase(),
      codeLower: card.cardCode.toLowerCase(),
      typeLower: cardType(card).toLowerCase().trim(),
      classSymbolsLower: card.classSymbols.map((cs) => cs.toLowerCase()),
      traitsLower: card.cardTraits.map((trait) => trait.toLowerCase()),
      encounterCode: match?.encounter_code,
    })
  }

  return index
})

const cardCounts = computed(() => {
  const bySet = new Map<string, number>()
  const byCycle = new Map<number, number>()
  const index = cardSearchIndex.value

  for (const card of allCards.value ?? []) {
    if (!cardInPool(card, cardPoolMode.value)) continue
    const meta = index.get(card.cardCode)
    if (meta?.setCode) bySet.set(meta.setCode, (bySet.get(meta.setCode) ?? 0) + 1)
    if (meta?.cycle) byCycle.set(meta.cycle, (byCycle.get(meta.cycle) ?? 0) + 1)
  }

  return { bySet, byCycle }
})

const cycleCount = (cycle: CardCycle) => cardCounts.value.byCycle.get(cycle.cycle) ?? 0

const expectedCardCount = (set: CardSet) => {
  if (set.homebrew) return setCount(set)

  const playerCards = set.playerCards
  const encounterCards = Math.max(encounterSetTotal(set) - playerCards, 0)
  if (cardPoolMode.value === 'player') return playerCards
  if (cardPoolMode.value === 'campaign') return encounterCards
  return playerCards + encounterCards
}

const cycleCountText = (cycle: CardCycle) => {
  if (!allCards.value) return 0
  const implementedCount = cycleCount(cycle)
  const cycleSets = setsByCycle.get(cycle.cycle) ?? []
  const total = cycleSets.reduce((acc, set) => acc + expectedCardCount(set), 0)

  if (implementedCount == total) {
    return ""
  }

  return ` (${implementedCount}/${total})`
}

const setCount = (set: CardSet) => cardCounts.value.bySet.get(set.code) ?? 0

const setCountText = (set: CardSet) => {
  const implementedCount = setCount(set)
  const total = expectedCardCount(set)

  if (implementedCount == total) {
    return ""
  }

  return ` (${implementedCount}/${total})`
}

const filteredCardsIgnoringPool = computed(() => {
  if (!allCards.value) return []

  const { classes, encounterSets, traits, cycle, set, text, level, cardTypes } = filter.value
  const classSet = classes.length > 0 ? new Set(classes) : null
  const traitSet = traits.length > 0 ? new Set(traits) : null
  const encounterSet = encounterSets.length > 0 ? new Set(encounterSets) : null
  const cardTypeSet = cardTypes.length > 0 ? new Set(cardTypes.map((ct) => ct.toLowerCase().trim())) : null
  const textLower = text.map((t) => t.toLowerCase())
  const codeText = textLower.map((t) => `c${t}`)
  const index = cardSearchIndex.value

  return allCards.value.filter((c) => {
    if (c.cardCode === "cx05184") return false

    const meta = index.get(c.cardCode)
    if (!meta) return false

    if (cycle && meta.cycle !== cycle) return false
    if (set === 'homebrew') {
      if (meta.cycle !== HOMEBREW_CYCLE) return false
    } else if (set && meta.setCode !== set) return false

    if (classSet && !meta.classSymbolsLower.some((cs) => classSet.has(cs))) return false
    if (traitSet && !meta.traitsLower.some((trait) => traitSet.has(trait))) return false

    if (encounterSet) {
      if (!meta.encounterCode || !encounterSet.has(meta.encounterCode)) return false
    }

    if (textLower.length > 0) {
      const cardNameMatches = textLower.some((term) => meta.nameLower.includes(term))
      const cardCodeMatches = codeText.some((term) => meta.codeLower === term)
      if (!cardNameMatches && !cardCodeMatches) return false
    }

    if (level && c.level !== level) return false
    if (cardTypeSet && !cardTypeSet.has(meta.typeLower)) return false

    return true
  })
})

const hasPlayerCards = computed(() => filteredCardsIgnoringPool.value.some((card) => !isCampaignCard(card)))
const hasCampaignCards = computed(() => filteredCardsIgnoringPool.value.some(isCampaignCard))
const canShowBothCards = computed(() => hasPlayerCards.value && hasCampaignCards.value)
const cardPoolAvailable = (mode: CardPoolMode) => {
  if (mode === 'player') return hasPlayerCards.value
  if (mode === 'campaign') return hasCampaignCards.value
  return canShowBothCards.value
}

const cards = computed(() => filteredCardsIgnoringPool.value.filter((c) => cardInPool(c, cardPoolMode.value)))

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
    cardTypes = matchCardTypes[1].split('|').map((s) => s.toLowerCase().trim())
  }

  const matchLevel = queryString.match(/p:([1-9][0-9]*)/)

  if (matchLevel) {
    queryString = queryString.replace(/p:([1-9][0-9]*)/, '')
    level = parseInt(matchLevel[1])
  }

  const matchClasses = queryString.match(/f:([^ ]*)/)

  if (matchClasses) {
    queryString = queryString.replace(/f:([^ ]*)/, '')
    classes = matchClasses[1].split('|').map((s) => s.toLowerCase().trim())
  }

  const matchCycle = queryString.match(/y:(-?\d+)/)

  if (matchCycle) {
    queryString = queryString.replace(/y:-?\d+/, '')
    const parsedCycle = parseInt(matchCycle[1])
    if (parsedCycle > 0) cycle = parsedCycle
  }

  const matchSet = queryString.match(/e:([^ ]*)/)

  if (matchSet) {
    queryString = queryString.replace(/e:([^ ]*)/, '')
    set = matchSet[1].toLowerCase()
  }

  const matchTraits = queryString.match(/k:([^ ]*)/)

  if (matchTraits) {
    queryString = queryString.replace(/k:([^ ]*)/, '')
    traits = matchTraits[1].split('|').map((s) => s.toLowerCase().trim())
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

  if (f.cycle && f.cycle !== HOMEBREW_CYCLE) {
    result += ` y:${f.cycle}`
  }

  if (f.cycle === HOMEBREW_CYCLE) {
    result += ' e:homebrew'
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

const cardSet = (card: Arkham.CardDef) => findCardSetByArt(card.art)

const cycleSets = (cycle: CardCycle) => setsByCycle.get(cycle.cycle) ?? []

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

function homebrewSetImagePath(code: string) {
  const homebrewId = code.replace(/^homebrew-/, '')
  return imgsrc(`homebrew/${homebrewId}/sets/${homebrewId}.png`)
}

function setIconSrc(set: CardSet) {
  return set.homebrew ? homebrewSetImagePath(set.code) : `/img/arkham/encounter-sets/${set.code}.png`
}

function cycleIconSrc(cycle: CardCycle) {
  if (cycle.cycle === HOMEBREW_CYCLE) {
    const set = cycleSets(cycle)[0]
    return set ? homebrewSetImagePath(set.code) : ''
  }
  const code = cycleIconCode(cycle)
  return code ? `/img/arkham/encounter-sets/${code}.png` : ''
}

const setCycle = (cycle: CardCycle) => {
  query.value = filterString({...filter.value, set: null, cycle: cycle.cycle})
  setFilter()
  showSidebar.value = false
}

const setSet = (set: CardSet) => {
  query.value = filterString({...filter.value, cycle: null, set: set.code})
  setFilter()
  showSidebar.value = false
}

const setCardPoolMode = (mode: CardPoolMode) => {
  if (!cardPoolAvailable(mode)) return

  router.push({
    name: 'Cards',
    query: {
      ...route.query,
      includeEncounter: undefined,
      cardPool: mode === 'player' ? undefined : mode,
    },
  })
}

watch([hasPlayerCards, hasCampaignCards, cardPoolMode], ([hasPlayer, hasCampaign, mode]) => {
  if (cardPoolAvailable(mode)) return
  if (hasCampaign) setCardPoolMode('campaign')
  else if (hasPlayer) setCardPoolMode('player')
}, { immediate: true })

const showSidebar = ref(false)
const sidebarCollapsed = ref(false)
</script>

<template>
  <div class="container">
    <div class="sidebar-overlay" :class="{ visible: showSidebar }" @click="showSidebar = false"></div>
    <div class="sidebar" :class="{ open: showSidebar, collapsed: sidebarCollapsed }">
      <button
        v-if="!sidebarCollapsed"
        class="sidebar-collapse"
        type="button"
        aria-label="Hide card sets"
        title="Hide card sets"
        @click="sidebarCollapsed = true"
      >
        <span class="collapse-glyph" aria-hidden="true" data-tooltip="Hide card sets">«</span>
      </button>
      <div class="sidebar-content">
      <button class="sidebar-close" @click="showSidebar = false"><font-awesome-icon icon="times" /></button>
      <div class="sidebar-card-pool card-pool-toggle segmented segmented-3" role="radiogroup" :aria-label="$t('cardsView.cardPool')">
        <input type="radio" :checked="cardPoolMode === 'player'" :disabled="!cardPoolAvailable('player')" id="card-pool-player-mobile" @change="setCardPoolMode('player')" />
        <label for="card-pool-player-mobile">{{ $t('cardsView.playerCards') }}</label>

        <input type="radio" :checked="cardPoolMode === 'campaign'" :disabled="!cardPoolAvailable('campaign')" id="card-pool-campaign-mobile" @change="setCardPoolMode('campaign')" />
        <label for="card-pool-campaign-mobile">{{ $t('cardsView.campaignCards') }}</label>

        <input type="radio" :checked="cardPoolMode === 'both'" :disabled="!cardPoolAvailable('both')" id="card-pool-both-mobile" @change="setCardPoolMode('both')" />
        <label for="card-pool-both-mobile">{{ $t('cardsView.bothCards') }}</label>
      </div>
      <div :class="['chapter-tabs segmented', dev ? 'segmented-3' : 'segmented-2']" role="radiogroup" aria-label="Card chapter">
        <input type="radio" :checked="activeChapter === 1" id="chapter-1" @change="activeChapter = 1" />
        <label for="chapter-1">{{ t('cardsView.chapter1') }}</label>
        <input type="radio" :checked="activeChapter === 2" id="chapter-2" @change="activeChapter = 2" />
        <label for="chapter-2">{{ t('cardsView.chapter2') }}</label>
        <template v-if="dev">
          <input type="radio" :checked="activeChapter === HOMEBREW_CYCLE" id="chapter-homebrew" @change="activeChapter = HOMEBREW_CYCLE" />
          <label for="chapter-homebrew">Homebrew</label>
        </template>
      </div>
      <nav class="cycles">
        <ol v-if="activeChapter === HOMEBREW_CYCLE">
          <li v-for="set in homebrewSets" :key="set.code">
            <div :class="['nav-row', 'nav-row--cycle', { active: filter.set === set.code }]">
              <span
                class="set-icon set-icon--homebrew"
                :style="{ '--set-icon-url': `url(${setIconSrc(set)})` }"
                role="img"
                :aria-label="set.name"
              ></span>
              <a href="#" @click.prevent="setSet(set)">{{set.name}}</a>
              <span class="count">{{setCountText(set)}}</span>
            </div>
          </li>
        </ol>
        <ol v-else>
          <li v-for="cycle in displayedCycles" :key="cycle.code">
            <div :class="['nav-row', 'nav-row--cycle', { active: filter.cycle === cycle.cycle }]">
              <i v-if="SET_FONT_CHARS[cycleIconCode(cycle)]" class="set-icon-font">{{ SET_FONT_CHARS[cycleIconCode(cycle)] }}</i>
              <img v-else-if="cycleIconSrc(cycle)" class="set-icon" :src="cycleIconSrc(cycle)" :alt="cycle.name" />
              <a href="#" @click.prevent="setCycle(cycle)">{{cycle.name}}</a>
              <span class="count">{{cycleCountText(cycle)}}</span>
            </div>
            <ol class="set-list">
              <li v-for="set in cycleSets(cycle)" :key="set.code">
                <div :class="['nav-row', 'nav-row--sub', { active: filter.set === set.code }]">
                  <i v-if="SET_FONT_CHARS[set.code]" class="set-icon-font">{{ SET_FONT_CHARS[set.code] }}</i>
                  <img v-else class="set-icon" :src="setIconSrc(set)" :alt="set.name" />
                  <a href="#" @click.prevent="setSet(set)">{{set.name}}</a>
                  <span class="count">{{setCountText(set)}}</span>
                </div>
              </li>
            </ol>
          </li>
        </ol>
      </nav>
      </div>
    </div>
    <div class="results">
      <header>
        <button
          v-if="sidebarCollapsed"
          class="desktop-sidebar-toggle"
          @click="sidebarCollapsed = false"
          title="Show card sets"
        >
          <font-awesome-icon class="toggle-arrow" icon="chevron-right" />
          <font-awesome-icon icon="book" />
        </button>
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
        <div class="desktop-card-pool card-pool-toggle segmented segmented-3" role="radiogroup" :aria-label="$t('cardsView.cardPool')">
          <input type="radio" :checked="cardPoolMode === 'player'" :disabled="!cardPoolAvailable('player')" id="card-pool-player" @change="setCardPoolMode('player')" />
          <label for="card-pool-player">{{ $t('cardsView.playerCards') }}</label>

          <input type="radio" :checked="cardPoolMode === 'campaign'" :disabled="!cardPoolAvailable('campaign')" id="card-pool-campaign" @change="setCardPoolMode('campaign')" />
          <label for="card-pool-campaign">{{ $t('cardsView.campaignCards') }}</label>

          <input type="radio" :checked="cardPoolMode === 'both'" :disabled="!cardPoolAvailable('both')" id="card-pool-both" @change="setCardPoolMode('both')" />
          <label for="card-pool-both">{{ $t('cardsView.bothCards') }}</label>
        </div>
      </header>
      <CardImageView v-if="view == View.Image" :cards="cards" :show-counts="false" />
      <CardListView v-if="view == View.List" :cards="cards" :show-counts="false" />
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
  position: relative;
  display: flex;
  flex-direction: column;
  width: clamp(260px, 21vw, 340px);
  border-right: 1px solid rgba(255,255,255,0.08);
  background: color-mix(in srgb, var(--background) 96%, black 4%);
  overflow: visible;
  z-index: 3;
  transition: width 0.18s ease, border-color 0.18s ease;

  &.collapsed {
    width: 0;
    border-right-color: transparent;

    .sidebar-content {
      display: none;
    }
  }

  @media (max-width: 768px) {
    position: fixed;
    right: 0;
    top: 0;
    bottom: 0;
    width: min(340px, 88vw);
    max-height: unset;
    border-right: none;
    border-left: 1px solid rgba(255,255,255,0.12);
    background: var(--background);
    z-index: var(--z-index-50);
    transform: translateX(100%);
    transition: transform 0.25s ease;
    overflow-y: auto;
    &.open { transform: translateX(0); }

    &.collapsed {
      width: min(340px, 88vw);

      .sidebar-content { display: flex; }
    }
  }
}

.sidebar-content {
  display: flex;
  flex: 1;
  min-width: 260px;
  min-height: 0;
  flex-direction: column;
  overflow: hidden;
}

.sidebar-collapse {
  position: absolute;
  top: 0;
  right: -9px;
  z-index: 4;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 18px;
  height: 100%;
  padding: 0;
  color: #aaa;
  background: transparent;
  border: 0;
  cursor: pointer;
  opacity: 0;
  transition: opacity 0.15s, color 0.15s;

  &:hover,
  &:focus-visible {
    opacity: 1;
    color: #fff;
  }

  .collapse-glyph {
    position: absolute;
    top: 50%;
    left: 50%;
    z-index: 1;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    color: #fff;
    font-size: 18px;
    font-weight: 800;
    letter-spacing: 0;
    line-height: 1;
    background: color-mix(in srgb, var(--background) 74%, white 26%);
    border: 1px solid rgba(255,255,255,0.22);
    border-radius: 999px;
    box-shadow: 0 4px 14px rgba(0,0,0,0.3);
    transform: translate(-50%, -54%);
    text-shadow: 0 1px 2px rgba(0,0,0,0.55);
  }

  .collapse-glyph:hover,
  &:focus-visible .collapse-glyph {
    background: color-mix(in srgb, var(--background) 72%, white 28%);
  }

  .collapse-glyph::after {
    content: attr(data-tooltip);
    position: absolute;
    top: 50%;
    left: 28px;
    z-index: 2;
    padding: 5px 8px;
    color: #eee;
    font-size: 0.72rem;
    font-weight: 600;
    line-height: 1;
    letter-spacing: 0;
    text-shadow: none;
    white-space: nowrap;
    pointer-events: none;
    background: rgba(12, 16, 18, 0.96);
    border: 1px solid rgba(255,255,255,0.14);
    border-radius: 6px;
    box-shadow: 0 8px 20px rgba(0,0,0,0.35);
    opacity: 0;
    transform: translateY(-50%) translateX(-4px);
    transition: opacity 0.12s, transform 0.12s;
  }

  .collapse-glyph:hover::after,
  &:focus-visible .collapse-glyph::after {
    opacity: 1;
    transform: translateY(-50%);
  }

  @media (max-width: 768px) {
    display: none;
  }
}

.sidebar:has(.sidebar-collapse:hover),
.sidebar:has(.sidebar-collapse:focus-visible) {
  border-right-color: rgba(255,255,255,0.35);
}

.sidebar-overlay {
  display: none;
  @media (max-width: 768px) {
    &.visible {
      display: block;
      position: fixed;
      inset: 0;
      background: rgba(0, 0, 0, 0.6);
      z-index: var(--z-index-49);
    }
  }
}

.sidebar-close {
  display: none;
  @media (max-width: 768px) {
    display: flex;
    align-self: flex-start;
    margin: 8px auto 0 8px;
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

.desktop-sidebar-toggle,
.sidebar-toggle {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 3px;
  flex-shrink: 0;
  height: 32px;
  padding: 0 8px;
  background: rgba(255,255,255,0.08);
  border: 1px solid rgba(255,255,255,0.15);
  border-radius: 6px;
  color: #aaa;
  cursor: pointer;
  &:hover { background: rgba(255,255,255,0.14); color: #eee; }
}

.sidebar-toggle {
  display: none;
  @media (max-width: 768px) {
    display: flex;
    order: 3;
  }
}

.desktop-sidebar-toggle {
  .toggle-arrow {
    display: inline-block;
    font-size: 0.65em;
    opacity: 0.7;
  }

  @media (max-width: 768px) {
    display: none;
  }
}

.toggle-arrow {
  display: none;
  @media (max-width: 768px) {
    display: inline-block;
    font-size: 0.65em;
    opacity: 0.7;
  }
}

.chapter-tabs {
  --segmented-items: 2;
  margin: 12px 12px 8px;
  flex-shrink: 0;
}

.cycles {
  flex: 1;
  overflow-y: auto;
  padding: 6px 10px 18px;
  scrollbar-color: rgba(255,255,255,0.22) transparent;

  ol {
    list-style: none;
    margin: 0;
    padding: 0;
  }

  > ol > li + li {
    margin-top: 4px;
  }

  &::-webkit-scrollbar-track,
  &::-webkit-scrollbar-corner {
    background: transparent;
  }
}

.nav-row {
  display: flex;
  align-items: center;
  overflow: hidden;
  min-height: 34px;
  padding: 0 10px;
  border-radius: 8px;
  transition: background 0.12s, color 0.12s;

  a {
    flex: 1;
    min-width: 0;
    padding: 7px 6px 7px 0;
    font-size: 0.84rem;
    font-weight: 600;
    color: #ccc;
    text-decoration: none;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    transition: color 0.12s;

    &:hover { color: var(--spooky-green); }
  }

  &.active {
    background: rgba(255,255,255,0.075);

    a,
    .set-icon-font,
    .count {
      color: var(--spooky-green);
    }

    .set-icon:not(.set-icon--homebrew) {
      filter: brightness(0) saturate(100%) invert(68%) sepia(55%) saturate(391%) hue-rotate(112deg) brightness(89%) contrast(88%);
    }

    .set-icon--homebrew {
      color: var(--spooky-green);
    }
  }

  .count {
    flex-shrink: 0;
    font-size: 0.72rem;
    color: var(--button);
    white-space: nowrap;
  }
}

.nav-row--cycle {
  min-height: 30px;
  margin: 0 4px 5px 0;

  a {
    padding-top: 5px;
    padding-bottom: 5px;
  }

  &:hover {
    background: rgba(255,255,255,0.045);
  }
}

.set-list {
  margin: 0 0 9px;
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

.set-icon--homebrew {
  width: 18px;
  height: 18px;
  margin-left: -1px;
  color: #fff;
  background: currentColor;
  filter: none;
  mask: var(--set-icon-url) center / contain no-repeat;
  -webkit-mask: var(--set-icon-url) center / contain no-repeat;
}

.nav-row--sub {
  min-height: 28px;
  margin-left: 30px;
  padding-left: 8px;

  a {
    padding-top: 5px;
    padding-bottom: 5px;
    font-size: 0.79rem;
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
  gap: 12px;
  flex-shrink: 0;
  padding: 14px 20px;
  background: color-mix(in srgb, var(--background) 92%, transparent);
  border-bottom: 1px solid rgba(255,255,255,0.07);
  backdrop-filter: blur(6px);
  z-index: var(--z-index-1);

  @media (max-width: 768px) {
    gap: 6px;
    padding: 8px max(8px, env(safe-area-inset-right)) 8px max(8px, env(safe-area-inset-left));
  }

  form {
    display: flex;
    align-items: center;
    background: rgba(255,255,255,0.06);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 6px;
    overflow: hidden;
    flex: 1;
    max-width: 360px;
    min-width: 0;

    @media (max-width: 768px) {
      max-width: none;
    }

    input {
      flex: 1;
      background: transparent;
      border: none;
      outline: none;
      padding: 6px 10px;
      color: #ddd;
      font-size: 0.88rem;

      &::placeholder { color: var(--button); }
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
  gap: 3px;
  background: rgba(255,255,255,0.05);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 8px;
  padding: 3px;

  button {
    background: transparent;
    border: none;
    border-radius: 4px;
    padding: 5px 9px;
    color: #777;
    cursor: pointer;
    transition: background 0.12s, color 0.12s;

    &:hover { color: #ccc; }

    :deep(svg) {
      display: block;
      width: 14px;
      height: 14px;
      font-size: 14px;
      max-width: 14px;
      max-height: 14px;
    }

    &.active {
      background: rgba(255,255,255,0.12);
      color: #eee;
    }
  }
}

.segmented {
  --segmented-gap: 2px;
  --segmented-padding: 2px;
  --segmented-items: 3;
  --segmented-gap-total: 4px;
  display: grid;
  border-radius: 5px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  padding: var(--segmented-padding);
  gap: var(--segmented-gap);
  position: relative;
}

.segmented::before {
  content: '';
  background: var(--button-1);
  border-radius: 3px;
  bottom: var(--segmented-padding);
  left: var(--segmented-padding);
  position: absolute;
  top: var(--segmented-padding);
  transform: translateX(0);
  transition: transform 220ms cubic-bezier(.2, .8, .2, 1), background 150ms ease;
  width: calc((100% - (var(--segmented-padding) * 2) - var(--segmented-gap-total)) / var(--segmented-items));
  z-index: 0;
}

.segmented:has(#card-pool-campaign:checked)::before,
.segmented:has(#card-pool-campaign-mobile:checked)::before,
.segmented:has(#chapter-2:checked)::before {
  transform: translateX(calc(100% + var(--segmented-gap)));
}

.segmented:has(#card-pool-both:checked)::before,
.segmented:has(#card-pool-both-mobile:checked)::before,
.segmented:has(#chapter-homebrew:checked)::before {
  transform: translateX(calc((100% + var(--segmented-gap)) * 2));
}

.segmented-2 {
  --segmented-items: 2;
  --segmented-gap-total: 2px;
  grid-template-columns: repeat(2, 1fr);
}

.segmented-3 { grid-template-columns: repeat(3, 1fr); }

.segmented input[type='radio'] {
  display: none;
}

.segmented label {
  align-items: center;
  border-radius: 3px;
  color: var(--background-light);
  cursor: pointer;
  display: flex;
  font-size: 11px;
  font-weight: 600;
  justify-content: center;
  letter-spacing: 0.06em;
  margin: 0;
  padding: 6px 8px;
  position: relative;
  text-transform: uppercase;
  transition: color 0.15s ease;
  user-select: none;
  white-space: nowrap;
  z-index: 1;
}

.segmented label:hover,
.segmented input[type='radio']:checked + label {
  color: var(--text);
}

.segmented input[type='radio']:disabled + label {
  color: color-mix(in srgb, var(--background-light) 45%, transparent);
  cursor: not-allowed;
}

.segmented input[type='radio']:disabled + label:hover {
  color: color-mix(in srgb, var(--background-light) 45%, transparent);
}

.segmented:hover::before {
  background: var(--button-1-highlight);
}

.card-pool-toggle {
  min-width: 255px;
}

.sidebar-card-pool {
  display: none;
}

@media (max-width: 768px) {
  .desktop-card-pool {
    display: none;
  }

  .sidebar-card-pool {
    display: grid;
    margin: 10px 12px 12px;
    min-width: 0;
  }

  header form {
    order: 1;
  }

  .view-controls {
    order: 2;
  }

  .sidebar-toggle :deep(svg),
  header form button :deep(svg) {
    width: 14px;
    height: 14px;
    font-size: 14px;
    max-width: 14px;
    max-height: 14px;
  }

  .view-controls {
    flex-shrink: 0;
  }

  .view-controls button {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 34px;
    height: 32px;
    padding: 0;
    line-height: 1;
  }
}

</style>
