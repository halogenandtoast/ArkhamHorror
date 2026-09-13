<script lang="ts" setup>
import { watch, ref, computed, provide, onMounted, onBeforeUnmount } from 'vue';
import { useI18n } from 'vue-i18n';
import { fetchCards, fetchHomebrewCards, type CardPoolMode } from '@/arkham/api';
import { useRouter, useRoute, LocationQueryValue } from 'vue-router';
import * as Arkham from '@/arkham/types/CardDef';
import CardListView from '@/arkham/components/CardListView.vue';
import CardImageView from '@/arkham/components/CardImageView.vue';
import CardDetailsModal from '@/arkham/components/CardDetailsModal.vue';
import SegmentedToggle from '@/components/SegmentedToggle.vue';
import sets from '@/arkham/data/sets.json'
import cycles from '@/arkham/data/cycles.json'
import { shallowRef } from 'vue';
import { useDbCardStore, ArkhamDBCard } from '@/stores/dbCards'
import { storeToRefs } from 'pinia'
import { isDevBuild } from '@/arkham/displayRules'
import { homebrewCampaigns } from '@/arkham/homebrewData'
import { useSettings } from '@/stores/settings'
import { byPrintedNumber, hasLibraryCards, libraryCards, librarySets, loadLibrary } from '@/arkham/customCardLibrary'
import { imgsrc, isTypingTarget } from '@/arkham/helpers'
import { cardGroupKey, groupCards } from '@/arkham/cardDetails'

const { t } = useI18n()

enum View {
  Image = "IMAGE",
  List = "LIST",
}

const CHAPTER_2_CYCLES = new Set([12, 13, 61])

/* The third chapter: cards that aren't printed cards. Two groups of them --
 * the homebrew campaigns this build ships, and the cards you built yourself --
 * each a cycle of its own, neither with a cycle number to take. The chapter is
 * still ?chapter=-1, as it was when homebrew was the only thing in it. */
const EXTRAS_CHAPTER = -1
const HOMEBREW_CYCLE = -1
const CUSTOM_CYCLE = -2
const HOMEBREW_SET_PREFIX = 'homebrew-'
const CUSTOM_SET_PREFIX = 'custom-'
// A group is filtered by its own name ('homebrew', 'custom'); one set within it
// by that name plus its id.
const isExtraSetFilter = (set: string | null) =>
  set === 'homebrew' ||
  set === 'custom' ||
  (set?.startsWith(HOMEBREW_SET_PREFIX) ?? false) ||
  (set?.startsWith(CUSTOM_SET_PREFIX) ?? false)

const dev = isDevBuild()
const { customCardsEnabled } = storeToRefs(useSettings())
if (customCardsEnabled.value) loadLibrary()

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
const routeChapter = route.query.chapter ? parseInt(route.query.chapter.toString()) : 1
const activeChapter = ref<number>(
  routeChapter === EXTRAS_CHAPTER && !dev && !customCardsEnabled.value ? 1 : routeChapter,
)

// Pressing `f` flips every card currently shown in image view. CardImage picks
// this up via inject and mirrors it into its own flipped state.
const flipAll = ref(false)
provide('cardFlipAll', flipAll)

/* Each printing as it was printed: the Core Set's own art under Core, the
 * revised art under Revised Core. The art preference in settings is about what
 * you play with, and says nothing about what a set contains. */
provide('cardIgnoreArtVariants', true)

const onKeydown = (event: KeyboardEvent) => {
  if (event.key !== 'f' && event.key !== 'F') return
  if (event.metaKey || event.ctrlKey || event.altKey) return
  if (view.value !== View.Image) return
  if (isTypingTarget(event.target)) return

  event.preventDefault()
  flipAll.value = !flipAll.value
}

onMounted(() => window.addEventListener('keydown', onKeydown))
onBeforeUnmount(() => window.removeEventListener('keydown', onKeydown))

const cardPoolMode = computed<CardPoolMode>(() => {
  const cardPool = route.query.cardPool?.toString()
  if (cardPool === 'campaign' || cardPool === 'both') return cardPool
  return route.query.includeEncounter === 'true' ? 'both' : 'player'
})
const store = useDbCardStore()

const CACHE_KEY_PREFIX = 'arkham_cards_cache_'
const CACHE_VERSION = 'v5'
const CACHE_TTL_MS = 5 * 60 * 1000 // 5 minutes

let cachedAllCards: Arkham.CardDef[] | null = null

const sortCards = (cards: Arkham.CardDef[]) => [...cards].sort((a, b) => {
  if (a.art < b.art) return -1
  if (a.art > b.art) return 1
  return 0
})

// Blood Token is an encounter card that belongs to no encounter set, so the usual
// test would file it with the player cards.
const setlessEncounterCards = new Set(['13119'])

const isCampaignCard = (card: Arkham.CardDef) => card.encounterSet != null || setlessEncounterCards.has(card.art)

/* The Revised Core Set reprints the original's encounter cards under numbers
 * 500 higher. The engine defines each of them once, under the original number,
 * so the browser makes the revised printing out of them -- otherwise the
 * revised set's campaign half reads as 79 cards that were never implemented.
 *
 * Only the number moves. Which picture each one shows is settled where every
 * other art is (`reprintedArt`): the few that were redrawn have art of their
 * own, and the rest are the original's. */
const REVISED_CORE_OFFSET = 500

const revisedCoreArt = (art: string) =>
  art.replace(/^\d+/, (digits) => String(parseInt(digits) + REVISED_CORE_OFFSET).padStart(5, '0'))

const revisedCoreCode = (code: string) => `c${revisedCoreArt(code.replace(/^c/, ''))}`

const coreSet = (sets as { code: string; min: number; max: number }[]).find((s) => s.code === 'core')

const revisedCorePrintings = (cards: Arkham.CardDef[]): Arkham.CardDef[] => {
  if (!coreSet) return []

  return cards
    .filter((card) => {
      if (!isCampaignCard(card)) return false
      const number = parseInt(card.art)
      return number >= coreSet.min && number <= coreSet.max
    })
    .map((card) => ({
      ...card,
      cardCode: revisedCoreCode(card.cardCode),
      art: revisedCoreArt(card.art),
      otherSide: card.otherSide ? revisedCoreCode(card.otherSide) : card.otherSide,
      meta: { ...card.meta, revisedFrom: card.art },
    }))
}

/* ArkhamDB never numbered these reprints, so one looks itself up under the
 * number it was made from -- otherwise it would carry no encounter set to
 * filter by and no translated name. */
const dbArt = (card: Arkham.CardDef): string => card.meta?.revisedFrom ?? card.art

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
  const sorted = sortCards([
    ...officialCards,
    ...revisedCorePrintings(officialCards),
    ...homebrewCards,
  ])
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
  code: string
  cycle: number
  // A homebrew campaign's cards, or a set from your own card library, rather
  // than a printed set.
  homebrew?: boolean
  custom?: boolean
  // Show every card code in [min, max] in image view, greying out the ones the
  // engine hasn't implemented yet. For sets still being built out.
  previewUnimplemented?: boolean
  // Unused code numbers within [min, max] that don't correspond to a real card,
  // so no placeholder is drawn for them.
  missing?: string[]
}

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
        code: `${HOMEBREW_SET_PREFIX}${id}`,
        cycle: HOMEBREW_CYCLE,
        homebrew: true,
      }
    })
  : []
const allCycles: CardCycle[] = dev ? [...cycles, homebrewCycle] : cycles
const allSets: CardSet[] = dev ? [...(sets as CardSet[]), ...homebrewSets] : (sets as CardSet[])

const customCycle: CardCycle = { name: 'Custom', cycle: CUSTOM_CYCLE, code: 'custom' }

/* Your own cards, grouped the way the library groups them: one nav entry per
 * set that has something in it. They are kept out of `allSets` because nothing
 * about a printed set applies to them -- no code range, no expected total, and
 * they come and go while the page is open. */
const customCardEntries = computed(() => {
  if (!customCardsEnabled.value) return []
  const setOrder = new Map(librarySets().map((set, index) => [set.id, index]))
  return libraryCards().sort(
    (a, b) => (setOrder.get(a.setId) ?? 0) - (setOrder.get(b.setId) ?? 0) || byPrintedNumber(a, b),
  )
})

const customCards = computed(() => customCardEntries.value.map((entry) => entry.def))

const customSetCode = (setId: string) => `${CUSTOM_SET_PREFIX}${setId.toLowerCase()}`

// The set filter is read out of the query string lowercased, so the codes it is
// compared against are lowercased when they are made.
const customSetCodeByCard = computed(() => {
  const map = new Map<string, string>()
  for (const entry of customCardEntries.value) map.set(entry.def.cardCode, customSetCode(entry.setId))
  return map
})

const customSets = computed<CardSet[]>(() => {
  const populated = new Set(customCardEntries.value.map((entry) => entry.setId))
  return librarySets()
    .filter((set) => populated.has(set.id))
    .map((set) => ({
      name: set.name,
      min: 0,
      max: 0,
      code: customSetCode(set.id),
      cycle: CUSTOM_CYCLE,
      custom: true,
    }))
})

const showCustomCards = computed(() => customCardsEnabled.value && hasLibraryCards.value)

/* What the third chapter lists: a group heading per cycle with its sets under
 * it, the same shape the printed chapters use. */
const extraGroups = computed(() => {
  const groups: { cycle: CardCycle; sets: CardSet[] }[] = []
  if (homebrewSets.length > 0) groups.push({ cycle: homebrewCycle, sets: homebrewSets })
  if (showCustomCards.value) groups.push({ cycle: customCycle, sets: customSets.value })
  return groups
})

const showExtrasChapter = computed(() => extraGroups.value.length > 0)

// Named for what is in it. A build with homebrew leads with that; a player who
// only has cards of their own sees the chapter called what it holds for them.
const extrasLabel = computed(() => (homebrewSets.length > 0 ? homebrewCycle.name : customCycle.name))

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
    const set = homebrewSets.find((s) => s.code === `${HOMEBREW_SET_PREFIX}${homebrewMatch[1]}`)
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
  if (newChapter === EXTRAS_CHAPTER) {
    query.value = filterString({ ...filter.value, cycle: null, set: extraGroups.value[0]?.cycle.code ?? null })
    setFilter()
  } else if (isExtraSetFilter(filter.value.set)) {
    // Leaving the chapter with one of its sets still selected would show an
    // empty chapter; land on the set the page opens with instead.
    query.value = filterString({ ...filter.value, cycle: null, set: 'core' })
    setFilter()
  }
})

watch(() => allCards.value, () => {
  const language = localStorage.getItem('language') || 'en'
  if (language === 'en') return
  if (!allCards.value) return

  for (const card of allCards.value) {
    const match: ArkhamDBCard | null = store.getDbCard(dbArt(card))
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
const displayedCycles = computed(() => activeChapter.value === 2 ? chapter2Cycles.value : chapter1Cycles.value)

// Everything the browser can show: the printed cards, plus your own.
const browsableCards = computed(() => {
  const official = allCards.value ?? []
  return customCards.value.length > 0 ? [...official, ...customCards.value] : official
})

const cardSearchIndex = computed(() => {
  const index = new Map<string, CardSearchIndex>()

  for (const card of browsableCards.value) {
    const customCode = customSetCodeByCard.value.get(card.cardCode)
    const set = customCode ? undefined : findCardSetByArt(card.art)
    const match: ArkhamDBCard | null = customCode ? null : store.getDbCard(dbArt(card))

    index.set(card.cardCode, {
      set,
      setCode: customCode ?? set?.code,
      cycle: customCode ? CUSTOM_CYCLE : set?.cycle,
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

  return browsableCards.value.filter((c) => {
    if (c.cardCode === "cx05184") return false

    const meta = index.get(c.cardCode)
    if (!meta) return false

    if (cycle && meta.cycle !== cycle) return false
    if (set === 'homebrew') {
      if (meta.cycle !== HOMEBREW_CYCLE) return false
    } else if (set === 'custom') {
      if (meta.cycle !== CUSTOM_CYCLE) return false
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

const cardPoolOptions = computed(() => ([
  { value: 'player' as CardPoolMode, label: t('cardsView.playerCards'), disabled: !cardPoolAvailable('player') },
  { value: 'campaign' as CardPoolMode, label: t('cardsView.campaignCards'), disabled: !cardPoolAvailable('campaign') },
  { value: 'both' as CardPoolMode, label: t('cardsView.bothCards'), disabled: !cardPoolAvailable('both') },
]))

const cards = computed(() => filteredCardsIgnoringPool.value.filter((c) => cardInPool(c, cardPoolMode.value)))

// A stand-in for a card the engine doesn't implement yet: enough of a CardDef
// for CardImage to show its art, and nothing else.
const unimplementedCard = (code: string, set: CardSet): Arkham.CardDef => ({
  cardCode: `unimplemented-${code}`,
  art: code,
  doubleSided: false,
  classSymbols: [],
  cardType: '',
  level: null,
  name: { title: code, subtitle: null },
  cardTraits: [],
  skills: [],
  cost: null,
  otherSide: null,
  meta: {},
  errata: null,
  encounterSet: set.code,
})

// Placeholders carry no searchable metadata, so they only make sense when the
// filter is nothing more than "show me this set".
const previewSets = computed(() => {
  const { set, cycle, text, level, cardTypes, classes, traits, encounterSets } = filter.value
  if (text.length || level || cardTypes.length || classes.length || traits.length || encounterSets.length) return []
  return allSets.filter((s) => {
    if (!s.previewUnimplemented) return false
    return set ? s.code === set : cycle ? s.cycle === cycle : false
  })
})

const unimplementedCards = computed(() => {
  if (!allCards.value) return []

  const implemented = new Set(allCards.value.map((c) => c.art.replace(/\D/g, '')))

  return previewSets.value.flatMap((set) => {
    const missing = new Set(set.missing ?? [])
    const placeholders: Arkham.CardDef[] = []

    for (let code = set.min; code <= set.max; code++) {
      const art = String(code)
      if (implemented.has(art) || missing.has(art)) continue
      placeholders.push(unimplementedCard(art, set))
    }

    return placeholders
  }).filter((c) => cardInPool(c, cardPoolMode.value))
})

const unimplementedArts = computed(() => new Set(unimplementedCards.value.map((c) => c.art)))

const imageViewCards = computed(() =>
  unimplementedCards.value.length === 0
    ? cards.value
    : sortCards([...cards.value, ...unimplementedCards.value]),
)

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
    // The extra chapters are filtered by set, never by cycle; an old link that
    // names one of their cycles is ignored rather than matching nothing.
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

// Opening straight onto the third chapter (a bookmark, a reload) with a printed
// set in the query would show that set under it.
if (activeChapter.value === EXTRAS_CHAPTER && !isExtraSetFilter(filter.value.set)) {
  query.value = filterString({ ...filter.value, cycle: null, set: extraGroups.value[0]?.cycle.code ?? null })
  setFilter()
}

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
  13: 'core',  // Small Campaign Expansions
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
  const homebrewId = code.replace(new RegExp(`^${HOMEBREW_SET_PREFIX}`), '')
  return imgsrc(`homebrew/${homebrewId}/sets/${homebrewId}.png`)
}

// Sets whose icon ships as an SVG rather than the usual PNG.
const SVG_SET_ICONS = new Set(['cob'])

const setIconPath = (code: string) =>
  `/img/arkham/encounter-sets/${code}.${SVG_SET_ICONS.has(code) ? 'svg' : 'png'}`

function setIconSrc(set: CardSet) {
  return set.homebrew ? homebrewSetImagePath(set.code) : setIconPath(set.code)
}

function cycleIconSrc(cycle: CardCycle) {
  const code = cycleIconCode(cycle)
  return code ? setIconPath(code) : ''
}

const setCycle = (cycle: CardCycle) => {
  query.value = filterString({...filter.value, set: null, cycle: cycle.cycle})
  setFilter()
  showSidebar.value = false
}

const setExtraGroup = (cycle: CardCycle) => {
  query.value = filterString({ ...filter.value, cycle: null, set: cycle.code })
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
const selectedCard = ref<Arkham.CardDef | null>(null)

// The details modal steps through the cards in the order the grid shows them,
// which groups two defs that are one physical card into a single tile.
const navigableCards = computed(() => groupCards(imageViewCards.value).map((entry) => entry.card))

const selectedIndex = computed(() => {
  if (!selectedCard.value) return -1
  const key = cardGroupKey(selectedCard.value)
  return navigableCards.value.findIndex((c) => cardGroupKey(c) === key)
})

const hasPrevCard = computed(() => selectedIndex.value > 0)
const hasNextCard = computed(() => selectedIndex.value >= 0 && selectedIndex.value < navigableCards.value.length - 1)

const stepCard = (delta: number) => {
  const card = navigableCards.value[selectedIndex.value + delta]
  if (card) selectedCard.value = card
}
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
      <SegmentedToggle class="sidebar-card-pool card-pool-toggle" :model-value="cardPoolMode" :options="cardPoolOptions" :label="$t('cardsView.cardPool')" @update:model-value="setCardPoolMode" />
      <div :class="['chapter-tabs segmented', showExtrasChapter ? 'segmented-3' : 'segmented-2']" role="radiogroup" aria-label="Card chapter">
        <input type="radio" :checked="activeChapter === 1" id="chapter-1" @change="activeChapter = 1" />
        <label for="chapter-1">{{ t('cardsView.chapter1') }}</label>
        <input type="radio" :checked="activeChapter === 2" id="chapter-2" @change="activeChapter = 2" />
        <label for="chapter-2">{{ t('cardsView.chapter2') }}</label>
        <template v-if="showExtrasChapter">
          <input type="radio" :checked="activeChapter === EXTRAS_CHAPTER" id="chapter-extras" @change="activeChapter = EXTRAS_CHAPTER" />
          <label for="chapter-extras">{{ extrasLabel }}</label>
        </template>
      </div>
      <nav class="cycles">
        <ol v-if="activeChapter === EXTRAS_CHAPTER && showExtrasChapter">
          <li v-for="group in extraGroups" :key="group.cycle.code">
            <div :class="['nav-row', 'nav-row--cycle', { active: filter.set === group.cycle.code }]">
              <font-awesome-icon class="set-icon-glyph" :icon="group.cycle.code === 'custom' ? 'flask' : 'wrench'" />
              <a href="#" @click.prevent="setExtraGroup(group.cycle)">{{ group.cycle.name }}</a>
            </div>
            <ol class="set-list">
              <li v-for="set in group.sets" :key="set.code">
                <div :class="['nav-row', 'nav-row--sub', { active: filter.set === set.code }]">
                  <span
                    v-if="set.homebrew"
                    class="set-icon set-icon--homebrew"
                    :style="{ '--set-icon-url': `url(${setIconSrc(set)})` }"
                    role="img"
                    :aria-label="set.name"
                  ></span>
                  <a href="#" @click.prevent="setSet(set)">{{set.name}}</a>
                </div>
              </li>
            </ol>
          </li>
        </ol>
        <ol v-else>
          <li v-for="cycle in displayedCycles" :key="cycle.code">
            <div :class="['nav-row', 'nav-row--cycle', { active: filter.cycle === cycle.cycle }]">
              <i v-if="SET_FONT_CHARS[cycleIconCode(cycle)]" class="set-icon-font">{{ SET_FONT_CHARS[cycleIconCode(cycle)] }}</i>
              <span
                v-else-if="cycleIconSrc(cycle)"
                class="set-icon"
                :style="{ '--set-icon-url': `url(${cycleIconSrc(cycle)})` }"
                role="img"
                :aria-label="cycle.name"
              ></span>
              <a href="#" @click.prevent="setCycle(cycle)">{{cycle.name}}</a>
            </div>
            <ol class="set-list">
              <li v-for="set in cycleSets(cycle)" :key="set.code">
                <div :class="['nav-row', 'nav-row--sub', { active: filter.set === set.code }]">
                  <i v-if="SET_FONT_CHARS[set.code]" class="set-icon-font">{{ SET_FONT_CHARS[set.code] }}</i>
                  <span
                    v-else
                    class="set-icon"
                    :style="{ '--set-icon-url': `url(${setIconSrc(set)})` }"
                    role="img"
                    :aria-label="set.name"
                  ></span>
                  <a href="#" @click.prevent="setSet(set)">{{set.name}}</a>
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
        <SegmentedToggle class="desktop-card-pool card-pool-toggle" :model-value="cardPoolMode" :options="cardPoolOptions" :label="$t('cardsView.cardPool')" @update:model-value="setCardPoolMode" />
      </header>
      <CardImageView
        v-if="view == View.Image"
        :cards="imageViewCards"
        :unimplemented="unimplementedArts"
        :show-counts="false"
        selectable
        @select="selectedCard = $event"
      />
      <CardListView v-if="view == View.List" :cards="cards" :show-counts="false" />
    </div>
    <CardDetailsModal
      v-if="selectedCard"
      :card="selectedCard"
      :unimplemented="unimplementedArts.has(selectedCard.art)"
      :has-prev="hasPrevCard"
      :has-next="hasNextCard"
      @prev="stepCard(-1)"
      @next="stepCard(1)"
      @close="selectedCard = null"
    />
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
    .set-icon,
    .set-icon-font {
      color: var(--spooky-green);
    }
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

/* Icons are silhouettes masked out of a solid fill, so they take the row's
   color exactly — matching the font glyphs the other rows use. */
.set-icon {
  width: 16px;
  height: 16px;
  flex-shrink: 0;
  margin-right: 4px;
  color: #ccc;
  background: currentColor;
  mask: var(--set-icon-url) center / contain no-repeat;
  -webkit-mask: var(--set-icon-url) center / contain no-repeat;
}

.set-icon--homebrew {
  width: 18px;
  height: 18px;
  margin-left: -1px;
  color: #fff;
}

/* A group heading with no set icon of its own. */
.set-icon-glyph {
  width: 16px;
  flex-shrink: 0;
  margin-right: 4px;
  color: #ccc;
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

.segmented:has(#chapter-2:checked)::before {
  transform: translateX(calc(100% + var(--segmented-gap)));
}

.segmented:has(#chapter-extras:checked)::before {
  transform: translateX(calc((100% + var(--segmented-gap)) * 2));
}

.segmented-2 {
  --segmented-items: 2;
  --segmented-gap-total: 2px;
  grid-template-columns: repeat(2, 1fr);
}

.segmented-3 {
  --segmented-items: 3;
  --segmented-gap-total: 4px;
  grid-template-columns: repeat(3, 1fr);
}

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
