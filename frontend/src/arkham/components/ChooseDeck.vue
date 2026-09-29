<script lang="ts" setup>
import { displayTabooId, displayTabooList } from '@/arkham/taboo';
import { computed, ref, inject, watch, nextTick } from 'vue'
import type { Game } from '@/arkham/types/Game';
import { fetchDecks } from '@/arkham/api'
import { cardImg, imgsrc, type InvestigatorClass } from '@/arkham/helpers'
import { bareCardCode, customCardDef, isCustomCardCode, stripCardCodePrefix } from '@/arkham/customCards'
import { overlayIsEmpty } from '@/arkham/deckOverlay'
import { hasLibraryCards, loadLibrary } from '@/arkham/customCardLibrary'
import { portraitImage as portraitImageHelper } from '@/arkham/cardImages'
import * as Arkham from '@/arkham/types/Deck'
import {deckClass} from '@/arkham/types/Deck'
import { deckInvestigatorCode, deckRequirementDescriptions, deckRestrictionError, hasValidatedUltimatumDeckConstraints, type SelectableDeckList } from '@/arkham/deckRestrictions'
import type { ArkhamDbDecklist, DeckMeta } from '@/arkham/types/Deck'
import type { Investigator } from '@/arkham/types/Investigator'
import Question from '@/arkham/components/Question.vue';
import OverlayEditor, { type DeckOverlay } from '@/arkham/components/debug/OverlayEditor.vue';
import { storeToRefs } from 'pinia';
import { useSettings } from '@/stores/settings';
import { useDbCardStore } from '@/stores/dbCards';
import UltimatumsAndBoonsQuestion from '@/arkham/components/UltimatumsAndBoonsQuestion.vue';
import LogIcons from '@/arkham/components/LogIcons.vue'
import NewDeck from '@/arkham/components/NewDeck.vue'
import DeckToolbar from '@/arkham/components/DeckToolbar.vue'
import { useI18n } from 'vue-i18n'
import { handleEmbeddedI18n } from '@/arkham/i18n'

const { t } = useI18n()
const dbCards = useDbCardStore()

const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const deckId = ref<string | null>(null)
const unsavedDeckList = ref<ArkhamDbDecklist | null>(null)
const createdPortrait = ref<string | null>(null)

type DeckType = "UseExistingDeck" | "LoadNewDeck" | "UnsavedDeck"

const deckType = ref<DeckType>("UseExistingDeck")

const searchText = ref('')
const filterClasses = ref<InvestigatorClass[]>([])
const sortBy = ref<Arkham.DeckSort>('name')
const validOnly = ref(false)

function deckPortraitCode(deck: Arkham.Deck): string {
  // The overlay edited here applies to this game only, but the row should still
  // show who you are about to play.
  if (deck.id === overlayFor.value && overlay.value?.investigator) {
    return stripCardCodePrefix(overlay.value.investigator)
  }
  return deckInvestigatorCode(Arkham.deckPlayList(deck))
}

// A laid-over deck plays differently from the one it was built as, so say so.
const deckHasOverlay = (deck: Arkham.Deck) => !overlayIsEmpty(deck.overlay ?? null)

function deckTaboo(deck: Arkham.Deck): string | null {
  const list = Arkham.deckPlayList(deck)
  return list.taboo_id ? displayTabooId(list.taboo_id) : null
}

const filteredDecks = computed(() => {
  const result = decks.value.filter((deck) => {
    const cls = deckClass(deck)
    const matchesClass = filterClasses.value.length === 0 ||
      filterClasses.value.some((k) => cls[k])
    const matchesSearch = !searchText.value ||
      deck.name.toLowerCase().includes(searchText.value.toLowerCase())
    const matchesValidity = !validOnly.value || deckError(Arkham.deckPlayList(deck)) === null
    return matchesClass && matchesSearch && matchesValidity
  })

  return Arkham.sortDecks(result, sortBy.value)
})

const props = defineProps<{
  game: Game
  playerId: string
}>()

const chooseDeck = inject<(deckId: string, overlay?: any) => Promise<void>>('chooseDeck')
const chooseDeckList = inject<(deckList: ArkhamDbDecklist) => Promise<void>>('chooseDeckList')
const question = computed(() => props.game.question[props.playerId])
const deckRequirements = computed(() => deckRequirementDescriptions(props.game.scenario?.id, {
  campaignId: props.game.campaign?.id,
  campaignLog: props.game.campaign?.log,
  ultimatumsAndBoons: props.game.settings.settingsUltimatumsAndBoons,
}, t))

// Deckbuilding Ultimatums restrict which decks qualify — like challenge
// scenarios, but stricter: default the list to valid decks only. The player
// can still toggle the filter off to see (error-annotated) invalid decks.
watch(
  () =>
    hasValidatedUltimatumDeckConstraints(
      props.game.settings.settingsUltimatumsAndBoons,
      !!props.game.campaign,
    ),
  (restricted) => {
    if (restricted) validOnly.value = true
  },
  { immediate: true },
)

const weaknessPoolOptions = [
  { token: 'cycle:core', label: 'Core Set', aliases: ['core'] },
  { token: 'cycle:rcore', label: 'Revised Core Set', aliases: ['rcore'] },
  { token: 'cycle:dwl', label: 'The Dunwich Legacy', aliases: ['dwl', 'dwlp'] },
  { token: 'cycle:ptc', label: 'The Path to Carcosa', aliases: ['ptc', 'ptcp'] },
  { token: 'cycle:tfa', label: 'The Forgotten Age', aliases: ['tfa', 'tfap'] },
  { token: 'cycle:tcu', label: 'The Circle Undone', aliases: ['tcu', 'tcup'] },
  { token: 'cycle:tde', label: 'The Dream-Eaters', aliases: ['tde', 'tdep'] },
  { token: 'cycle:tic', label: 'The Innsmouth Conspiracy', aliases: ['tic', 'ticp'] },
  { token: 'cycle:eote', label: 'Edge of the Earth', aliases: ['eote', 'eoep'] },
  { token: 'cycle:tsk', label: 'The Scarlet Keys', aliases: ['tsk', 'tskp'] },
  { token: 'cycle:fhv', label: 'The Feast of Hemlock Vale', aliases: ['fhv', 'fhvp'] },
  { token: 'cycle:tdc', label: 'The Drowned City', aliases: ['tdc', 'tdcp'] },
  { token: 'cycle:core_ch2', label: 'Chapter 2 Core Set', aliases: ['core_ch2', 'core2026', 'core_2026'] },
  { token: 'cycle:return', label: 'Return To boxes', aliases: ['return'] },
  { token: 'cycle:investigator_decks', label: 'Investigator Starter Decks', aliases: ['investigator_decks'] },
  { token: 'cycle:investigator_decks_ch2', label: 'Chapter 2 Investigator Decks', aliases: ['investigator_decks_ch2'] },
]

function normalizeWeaknessPoolToken(token: string): string {
  const trimmed = token.trim()
  const bare = trimmed.startsWith('cycle:') ? trimmed.slice(6) : trimmed
  return weaknessPoolOptions.find((o) => o.token === trimmed || o.aliases.includes(bare))?.token ?? trimmed
}

const selectedWeaknessPool = ref<string[]>([])
const weaknessPoolTouched = ref(false)
const weaknessPoolOpen = ref(false)

const selectedDeck = computed(() => decks.value.find((d) => d.id === deckId.value) ?? null)
const currentDeckList = computed(() => {
  if (unsavedDeckList.value) return unsavedDeckList.value
  if (!selectedDeck.value) return null
  return deckToArkhamDbDecklist(selectedDeck.value)
})
const weaknessPoolSummary = computed(() => {
  if (selectedWeaknessPool.value.length === 0) return 'All basic weaknesses'
  const names = selectedWeaknessPool.value.map((token) => weaknessPoolOptions.find((o) => o.token === token)?.label ?? token)
  return names.length <= 2 ? names.join(', ') : `${names.length} products selected`
})

function deckToArkhamDbDecklist(deck: Arkham.Deck): ArkhamDbDecklist {
  const list = Arkham.deckPlayList(deck)
  return {
    id: deck.id,
    name: deck.name,
    url: deck.url,
    investigator_code: list.investigator_code,
    investigator_name: deck.name,
    slots: { ...list.slots },
    sideSlots: { ...(list.sideSlots ?? {}) },
    meta: list.meta,
    taboo_id: list.taboo_id ?? null,
  }
}

function decodeMeta(meta: DeckMeta | undefined): Record<string, unknown> {
  if (!meta) return {}
  if (typeof meta === 'string') {
    try {
      const parsed = JSON.parse(meta)
      return parsed && typeof parsed === 'object' && !Array.isArray(parsed) ? parsed : {}
    } catch (_e) {
      return {}
    }
  }
  return { ...meta }
}

function weaknessPoolFromMeta(meta: DeckMeta | undefined): string[] {
  const cardPool = decodeMeta(meta).card_pool
  if (typeof cardPool !== 'string') return []
  return [...new Set(cardPool.split(',').map(normalizeWeaknessPoolToken).filter(Boolean))]
}

function deckListWithWeaknessPool(deckList: ArkhamDbDecklist): ArkhamDbDecklist {
  const meta = decodeMeta(deckList.meta)
  if (selectedWeaknessPool.value.length === 0) {
    delete meta.card_pool
  } else {
    meta.card_pool = selectedWeaknessPool.value.join(',')
  }

  return { ...deckList, meta: JSON.stringify(meta) }
}

function resetWeaknessPoolFromDeck() {
  selectedWeaknessPool.value = weaknessPoolFromMeta(currentDeckList.value?.meta)
  weaknessPoolTouched.value = false
  weaknessPoolOpen.value = false
}

function setWeaknessPool(tokens: string[]) {
  selectedWeaknessPool.value = tokens
  weaknessPoolTouched.value = true
}

async function toggleWeaknessPoolForDeck(deck: Arkham.Deck) {
  if (deckId.value === deck.id) {
    weaknessPoolOpen.value = !weaknessPoolOpen.value
    return
  }

  deckId.value = deck.id
  await nextTick()
  weaknessPoolOpen.value = true
}

watch(currentDeckList, resetWeaknessPoolFromDeck)

const questionLabel = computed(() => {
  if (question.value)
    return question.value.tag === 'QuestionLabel' ? handleEmbeddedI18n(question.value.label, t) : null
})

// Ultimatums/Boons deckbuilding interruptions (e.g. Boon of the Morrígan's
// weakness choice) get a dedicated, boon-styled question UI.
const isUltimatumsAndBoonsQuestion = computed(() =>
  question.value?.tag === 'QuestionLabel'
    && question.value.label?.startsWith('$label.ultimatumsAndBoons')
)

async function setPortrait(src: string) {
  createdPortrait.value = src
}

async function addDeck(d: Arkham.Deck) {
  decks.value = [...decks.value, d]
  deckId.value = d.id
  unsavedDeckList.value = null
  deckType.value = "UseExistingDeck"
  // "Save and use" means use it: seat the deck immediately rather than dropping the player
  // back into the existing-deck list to hunt for what they just made (a filter or search
  // could even be hiding it). Reset the pool state up front instead of waiting on the
  // currentDeckList watcher -- it flushes after this function, so a pool left selected on a
  // PREVIOUS deck would otherwise be applied to this one. Same for the overlay.
  resetWeaknessPoolFromDeck()
  resetOverlayFromDeck()
  await choose()
}

async function addUnsavedDeck(dl: ArkhamDbDecklist) {
  unsavedDeckList.value = dl
  deckId.value = null
  deckType.value = "UnsavedDeck"
}

// A seat joining a campaign already in progress may only take an investigator
// nobody has played this campaign; the ask carries the list.
const usedInvestigators = computed<string[]>(() => {
  const q = props.game.question[props.playerId]
  const inner = q?.tag === 'QuestionLabel' ? q.question : q
  return inner?.tag === 'ChooseJoinDeck' ? inner.usedInvestigators : []
})

function deckUsedThisCampaign(deckList: SelectableDeckList): boolean {
  return usedInvestigators.value.includes(deckInvestigatorCode(deckList))
}

/* Who an investigator is, rather than which printing of them you own: a
 * parallel front and the original are the same person and cannot both sit at
 * the table. Game state serialises codes with a leading 'c' while a decklist
 * carries the bare code, so neither side can be compared raw -- both resolve
 * through the card store to a name instead. */
function investigatorIdentity(cardCode: string): string {
  if (isCustomCardCode(cardCode)) {
    return (customCardDef(cardCode)?.name?.title ?? bareCardCode(cardCode)).toLowerCase()
  }
  const code = bareCardCode(cardCode)
  return (dbCards.getDbCard(code)?.real_name ?? code).toLowerCase()
}

const seatedIdentities = computed(
  () => new Set(Object.values(props.game.investigators).map((i) => investigatorIdentity(i.cardCode)))
)

const otherScenarioIdentities = computed(
  () => new Set(Object.values(props.game.otherInvestigators).map((i) => investigatorIdentity(i.id)))
)

function deckInvestigatorTaken(deckList: SelectableDeckList): boolean {
  return seatedIdentities.value.has(investigatorIdentity(deckInvestigatorCode(deckList)))
}

// Anything the table cannot seat: the row dims and its "use" button is dead,
// rather than letting the pick through to an error.
function deckUnavailable(deckList: SelectableDeckList): boolean {
  return deckUsedThisCampaign(deckList) || deckInvestigatorTaken(deckList)
}

function deckUnavailableReason(deckList: SelectableDeckList): string | undefined {
  if (deckUsedThisCampaign(deckList)) return t('chooseDeck.alreadyPlayedThisCampaign')
  if (deckInvestigatorTaken(deckList)) return t('chooseDeck.investigatorAlreadyChosen')
  return undefined
}

function deckError(deckList: SelectableDeckList): string | null {
  if (deckUsedThisCampaign(deckList)) {
    return t('chooseDeck.alreadyPlayedThisCampaign')
  }

  const chosenInvestigatorCodes = Object.values(props.game.investigators).map((i) => i.cardCode)
  const restrictionError = deckRestrictionError(props.game.scenario?.id, deckList, chosenInvestigatorCodes, {
    campaignId: props.game.campaign?.id,
    campaignLog: props.game.campaign?.log,
    // Deck legality follows the SELECTED tags, not the runtime on/off toggle.
    ultimatumsAndBoons: props.game.settings.settingsUltimatumsAndBoons,
  }, t, { isLastPlayer: isLastPlayerChoosing.value })
  if (restrictionError) return restrictionError

  if (deckInvestigatorTaken(deckList)) {
    return t('chooseDeck.investigatorAlreadyChosen')
  }

  if (otherScenarioIdentities.value.has(investigatorIdentity(deckInvestigatorCode(deckList)))) {
    return t('chooseDeck.investigatorInAnotherScenario')
  }

  return null
}

const error = computed(() => {
  if(!deckId.value) {
    return null
  }

  const deck = decks.value.find((d) => d.id === deckId.value)
  return deck ? deckError(Arkham.deckPlayList(deck)) : null
})

const unsavedDeckError = computed(() => {
  return unsavedDeckList.value ? deckError(unsavedDeckList.value) : null
})

const investigators = computed(() => props.game.investigators)

fetchDecks().then((result) => {
  decks.value = result;
  if (result.length == 0) {
    deckType.value = "LoadNewDeck"
  }
  ready.value = true;
})

const settings = useSettings()
const { customCardsEnabled } = storeToRefs(settings)

// A deck already laid over with your cards needs them resolvable to draw its row.
if (customCardsEnabled.value) loadLibrary()

/* Applies to this game only: it rides along with the answer rather than being
 * saved onto the deck.
 *
 * An overlay is built against one deck's slots -- the signatures it takes out
 * are that deck's -- so it belongs to that deck and nothing else.
 * `overlayFor` is what says which, and everything reading `overlay` checks it. */
const overlay = ref<DeckOverlay | null>(null)
const overlayFor = ref<string | null>(null)
const overlayOpen = ref(false)

function resetOverlayFromDeck() {
  overlay.value = null
  overlayFor.value = null
  overlayOpen.value = false
}

/* The overlay of the deck currently selected, or nothing. */
const selectedOverlay = computed(() =>
  overlayFor.value !== null && overlayFor.value === deckId.value ? overlay.value : null
)

async function toggleOverlayForDeck(deck: Arkham.Deck) {
  if (deckId.value === deck.id) {
    overlayOpen.value = !overlayOpen.value
    overlayFor.value = deck.id
    return
  }

  deckId.value = deck.id
  // The reset watcher flushes on the deck change; opening before it does would
  // close the panel this click is opening.
  await nextTick()
  overlayFor.value = deck.id
  overlayOpen.value = true
}

// Same trigger as the weakness pool: a deck change drops what was built for the
// deck before it.
watch(currentDeckList, resetOverlayFromDeck)

const overlaySummary = computed(() => {
  const o = selectedOverlay.value
  if (!o) return 'none'
  const parts: string[] = []
  if (o.investigator) parts.push('investigator')
  const cards =
    Object.keys(o.swaps).length + Object.keys(o.add).length + Object.keys(o.remove).length
  if (cards) parts.push(`${cards} card${cards === 1 ? '' : 's'}`)
  return parts.join(', ') || 'none'
})

const emit = defineEmits(['choose'])

const chooseChoice = (idx: number) => emit('choose', idx)

async function choose() {
  if (unsavedDeckList.value && chooseDeckList && unsavedDeckError.value === null) {
    await chooseDeckList(deckListWithWeaknessPool(unsavedDeckList.value))
  } else if (deckId.value && error.value === null) {
    if (weaknessPoolTouched.value && chooseDeckList && selectedDeck.value) {
      await chooseDeckList(deckListWithWeaknessPool(deckToArkhamDbDecklist(selectedDeck.value)))
    } else if (chooseDeck) {
      await chooseDeck(deckId.value, selectedOverlay.value)
    }
  }
}

async function selectAndChoose(deck: Arkham.Deck) {
  deckId.value = deck.id
  if (error.value !== null) return
  await choose()
}

type Player = { tag: "EmptyPlayer", id: string } | { tag: "Chosen", contents: Investigator, id: string }

const tabooList = function (investigator: Investigator) {
  return investigator.taboo ? displayTabooList(investigator.taboo) : null
}

const players = computed<Player[]>(() => {
  if (props.game.gameState.tag !== 'IsChooseDecks') return []

  const seated = Object.values(investigators.value)
  // A seat joining mid-campaign is the only pending one, so show the investigators
  // already at the table alongside it rather than an otherwise empty screen.
  const pending = props.game.gameState.contents
  const ids = [...pending, ...seated.map((i) => i.playerId).filter((p) => !pending.includes(p))]

  return ids.map((p) => {
    const maybeInvestigator = seated.find((i) => i.playerId === p)
    return maybeInvestigator ? { tag: "Chosen", contents: maybeInvestigator, id: p } : { tag: "EmptyPlayer", id: p }
  })
})

const chosenCount = computed(() => players.value.filter((p) => p.tag === 'Chosen').length)

// A challenge scenario only needs one player to use the required deck. The
// required investigator is therefore only enforced on the final player still
// choosing, and only if nobody else has already provided it.
const isLastPlayerChoosing = computed(() =>
  players.value.filter((p) => p.tag === 'EmptyPlayer').length <= 1
)

function portraitImage(investigator: Investigator) {
  return portraitImageHelper(investigator.cardCode)
}

const needsReply = computed(() => {
  const question = props.game.question[props.playerId]
  if (question === null || question === undefined) {
    return false
  }

  const inner = question.tag === 'QuestionLabel' ? question.question : question
  return inner.tag === 'ChooseDeck' || inner.tag === 'ChooseJoinDeck'
})



</script>

<template>
  <div class="container scroll-container">
    <LogIcons />
    <div class="investigators">
      <h2 class="page-title">{{$t('create.chooseYourDeck', players.length)}}</h2>
      <p v-if="players.length > 1" class="page-progress">
        {{ $t('create.decksChosen', { chosen: chosenCount, total: players.length }) }}
      </p>
      <div class="portraits">
        <div
          class="investigator-row"
          :class="{ 'investigator-row--choosing': needsReply && player.id == playerId }"
          v-for="(player, index) in players"
          :key="player.id"
        >
          <template v-if="player.tag === 'Chosen'">
            <!-- Setup still wants something from this seat, so the investigator
                 becomes a sidebar for the question rather than a summary. -->
            <template v-if="question && playerId == player.contents.playerId">
              <div class="seated">
                <div class="portrait">
                  <img :src="portraitImage(player.contents)" />
                </div>
                <div class="seated-stats">
                  <span class="stat-chip stat-health">
                    <svg class="icon"><use xlink:href="#icon-health"></use></svg>
                    <span class="stat-value">{{ player.contents.health }}</span>
                  </span>
                  <span class="stat-chip stat-sanity">
                    <svg class="icon"><use xlink:href="#icon-sanity"></use></svg>
                    <span class="stat-value">{{ player.contents.sanity }}</span>
                  </span>
                </div>
                <p class="seated-name">{{ player.contents.name.title }}</p>
              </div>
              <div class="question">
                <UltimatumsAndBoonsQuestion
                  v-if="isUltimatumsAndBoonsQuestion"
                  :game="game"
                  :playerId="playerId"
                  @choose="chooseChoice"
                />
                <template v-else>
                  <h2 v-if="questionLabel" class="title question-label">{{ questionLabel }}</h2>
                  <Question :game="game" :playerId="playerId" @choose="chooseChoice" />
                </template>
              </div>
            </template>
            <!-- Settled: nothing is being asked, so the seat reads across the
                 row at the same height as one still waiting for a deck. -->
            <div v-else class="seated-summary">
              <img class="seated-summary-portrait" :src="portraitImage(player.contents)" :alt="player.contents.name.title" />
              <div class="seated-summary-text">
                <span class="seated-summary-name">{{ player.contents.name.title }}</span>
                <span v-if="tabooList(player.contents)" class="taboo-list">
                  {{$t('create.tabooList', {tabooList: tabooList(player.contents)})}}
                </span>
              </div>
              <div class="seated-summary-stats">
                <span class="stat-chip stat-health">
                  <svg class="icon"><use xlink:href="#icon-health"></use></svg>
                  <span class="stat-value">{{ player.contents.health }}</span>
                </span>
                <span class="stat-chip stat-sanity">
                  <svg class="icon"><use xlink:href="#icon-sanity"></use></svg>
                  <span class="stat-value">{{ player.contents.sanity }}</span>
                </span>
              </div>
            </div>
          </template>
          <template v-else-if="needsReply && player.id == playerId">
            <div class="deck-main">
              <div v-if="deckRequirements.length" class="deck-requirements-card">
                <div class="deck-requirements-title">Deck Requirements</div>
                <div class="deck-requirements-body">
                  <ul class="deck-requirements">
                    <li v-for="requirement in deckRequirements" :key="requirement">{{ requirement }}</li>
                  </ul>
                  <button
                    type="button"
                    class="valid-filter"
                    :class="{ active: validOnly }"
                    @click.prevent="validOnly = !validOnly"
                  >
                    {{ validOnly ? 'Showing Valid Decks' : 'Filter Valid Decks' }}
                  </button>
                </div>
              </div>
              <div class="deck-tabs">
                <button @click.prevent="deckType = 'UseExistingDeck'" :class="{ current: deckType == 'UseExistingDeck'}" :disabled="decks.length == 0">
                  {{$t('create.useExistingDeck')}}
                </button>
                <button @click.prevent="deckType = 'LoadNewDeck'" :class="{ current: deckType == 'LoadNewDeck' || deckType == 'UnsavedDeck'}">
                  {{$t('create.loadNewDeck')}}
                </button>
              </div>
              <div v-if="deckType == 'UseExistingDeck'" class="deck-picker">
                <DeckToolbar
                  compact
                  :search-placeholder="$t('chooseDeck.search')"
                  v-model:search="searchText"
                  v-model:filterClasses="filterClasses"
                  v-model:sortBy="sortBy"
                />
                <div class="deck-list">
                  <div v-if="filteredDecks.length === 0" class="deck-list-empty">{{ $t('noDecksMatchFilters') }}</div>
                  <template v-for="deck in filteredDecks" :key="deck.id">
                    <div
                      class="deck-item"
                      :class="[deckClass(deck), { selected: deckId === deck.id, 'has-error': deckId === deck.id && error, 'deck-item--used': deckUnavailable(deck.list) }]"
                      v-tooltip="deckUnavailableReason(deck.list)"
                      @click.prevent="deckId = deck.id"
                    >
                      <img class="deck-item-portrait" :src="cardImg(deckPortraitCode(deck))" />
                      <div class="deck-item-info">
                        <span class="deck-item-name">{{ deck.name }}</span>
                        <span v-if="deckTaboo(deck)" class="deck-item-taboo">
                          <font-awesome-icon icon="book" /> {{ deckTaboo(deck) }}
                        </span>
                        <span
                          v-if="deckHasOverlay(deck)"
                          class="deck-item-overlaid"
                          title="This deck is laid over with custom cards"
                        >
                          <font-awesome-icon icon="layer-group" /> Overlay
                        </span>
                        <span v-if="deckId === deck.id && error" class="deck-item-error">{{ error }}</span>
                      </div>
                      <button
                        v-if="customCardsEnabled && hasLibraryCards"
                        type="button"
                        class="deck-item-overlay"
                        :class="{ active: overlayFor === deck.id && overlay }"
                        :title="`Overlay: ${overlaySummary}`"
                        @click.stop.prevent="toggleOverlayForDeck(deck)"
                      >
                        <font-awesome-icon icon="layer-group" />
                      </button>
                      <button
                        type="button"
                        class="deck-item-weakness-button"
                        :class="{ active: deckId === deck.id && (weaknessPoolOpen || selectedWeaknessPool.length > 0) }"
                        v-tooltip="$t('chooseDeck.randomBasicWeaknessPoolTooltip')"
                        :aria-label="$t('chooseDeck.randomBasicWeaknessPoolTooltip')"
                        @click.stop.prevent="toggleWeaknessPoolForDeck(deck)"
                      >
                        <font-awesome-icon icon="shuffle" />
                      </button>
                      <button class="deck-item-use" :disabled="deckUnavailable(deck.list)" @click.stop.prevent="selectAndChoose(deck)" :title="$t('chooseDeck.useThisDeck')">
                        <font-awesome-icon icon="chevron-right" />
                      </button>
                      <div v-if="overlayFor === deck.id && overlayOpen" class="weakness-pool-panel deck-item-weakness-pool" @click.stop>
                        <div class="weakness-pool-heading">
                          <span>Overlay</span>
                          <span class="weakness-pool-summary">{{ overlaySummary }}</span>
                        </div>
                        <p class="weakness-pool-help">
                          Custom cards, laid over this deck for this game only. The deck itself is
                          not changed.
                        </p>
                        <OverlayEditor v-model="overlay" :slots="deck.list.slots" :investigator="deck.list.investigator_code" />
                      </div>
                      <div v-if="deckId === deck.id && weaknessPoolOpen" class="weakness-pool-panel deck-item-weakness-pool" @click.stop>
                        <div class="weakness-pool-heading">
                          <span>Random basic weakness pool</span>
                          <span class="weakness-pool-summary">{{ weaknessPoolSummary }}</span>
                        </div>
                        <p class="weakness-pool-help">
                          Limit random basic weaknesses to selected products. Leave empty to use the full pool.
                        </p>
                        <div class="weakness-pool-actions">
                          <button type="button" @click.prevent="setWeaknessPool(weaknessPoolOptions.map((o) => o.token))">Select all</button>
                          <button type="button" @click.prevent="setWeaknessPool([])">Clear</button>
                        </div>
                        <div class="weakness-pool-grid">
                          <label v-for="option in weaknessPoolOptions" :key="option.token" class="weakness-pool-option">
                            <input type="checkbox" :value="option.token" v-model="selectedWeaknessPool" @change="weaknessPoolTouched = true" />
                            <span>{{ option.label }}</span>
                          </label>
                        </div>
                      </div>
                    </div>
                  </template>
                </div>
              </div>
              <div v-else class="load-deck-layout">
                <div class="load-deck-portrait">
                  <div v-if="createdPortrait" class="portrait">
                    <img :src="createdPortrait" />
                  </div>
                  <div v-else class="portrait-empty">
                    <img :src="imgsrc('slots/ally.png')" />
                  </div>
                </div>
                <div class="load-deck-content">
                  <form v-if="deckType == 'UnsavedDeck'" class="deck-form" @submit.prevent="choose">
                    <p class="unsaved-deck-name">{{ unsavedDeckList?.name }}</p>
                    <p v-if="unsavedDeckError" class="deck-form-error">{{ unsavedDeckError }}</p>
                    <div class="weakness-pool-panel">
                      <button type="button" class="weakness-pool-toggle" @click.prevent="weaknessPoolOpen = !weaknessPoolOpen">
                        <span>Random basic weakness pool</span>
                        <span class="weakness-pool-summary">{{ weaknessPoolSummary }}</span>
                      </button>
                      <div v-if="weaknessPoolOpen" class="weakness-pool-body">
                        <p class="weakness-pool-help">
                          Limit random basic weaknesses to selected products. Leave empty to use the full pool.
                        </p>
                        <div class="weakness-pool-actions">
                          <button type="button" @click.prevent="setWeaknessPool(weaknessPoolOptions.map((o) => o.token))">Select all</button>
                          <button type="button" @click.prevent="setWeaknessPool([])">Clear</button>
                        </div>
                        <div class="weakness-pool-grid">
                          <label v-for="option in weaknessPoolOptions" :key="option.token" class="weakness-pool-option">
                            <input type="checkbox" :value="option.token" v-model="selectedWeaknessPool" @change="weaknessPoolTouched = true" />
                            <span>{{ option.label }}</span>
                          </label>
                        </div>
                      </div>
                    </div>
                    <button type="submit" class="primary-action" :disabled="!!unsavedDeckError">{{$t('create.choose')}}</button>
                  </form>
                  <NewDeck v-else @new-deck="addDeck" @new-deck-list="addUnsavedDeck" :no-portrait="true" :set-portrait="setPortrait" />
                </div>
              </div>
            </div>
          </template>
          <template v-else>
            <div class="seat-pending">
              <div class="seat-pending-portrait">
                <img :src="imgsrc('slots/ally.png')" alt="" />
              </div>
              <div class="seat-pending-text">
                <span class="seat-pending-name">{{ $t('create.seatNumber', { number: index + 1 }) }}</span>
                <span class="seat-pending-status">{{ $t('create.seatNoDeckYet') }}</span>
              </div>
            </div>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>


<style scoped>
.container {
  background: var(--background);
  width: 100%;
  max-width: unset;
  height: 100%;
  margin: 0;
}

.investigators {
  width: 100%;
  color: #FFF;
  padding: 10px;
  border-radius: 3px;
  max-width: 800px;
  margin-inline: auto;
  margin-top: 20px;
}

.page-title {
  margin: 0 0 12px 0;
  padding: 0;
  text-transform: uppercase;
  color: var(--title);
  font-family: Teutonic;
  font-size: 1.8em;
  letter-spacing: 0.04em;
}

.page-progress {
  margin: -6px 0 12px 0;
  color: rgba(255, 255, 255, 0.45);
  font-size: 0.78em;
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.portraits {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

/* A seat nobody has filled yet still belongs to the table, so it keeps the row
   shape rather than collapsing to an empty box -- but it is secondary, so it
   sits at a fraction of the height of the seat actually being chosen for. */
.seat-pending {
  display: flex;
  align-items: center;
  gap: 12px;
  opacity: 0.55;
}

.seat-pending-portrait {
  width: 40px;
  height: 58px;
  flex-shrink: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 4px;
  background: rgba(0, 0, 0, 0.25);
  border: 1px dashed rgba(255, 255, 255, 0.14);

  img {
    width: 55%;
    opacity: 0.5;
  }
}

.seat-pending-text {
  display: flex;
  flex-direction: column;
  gap: 3px;
  min-width: 0;
}

.seat-pending-name {
  font-size: 0.86em;
  font-weight: 700;
  letter-spacing: 0.04em;
}

.seat-pending-status {
  font-size: 0.74em;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: rgba(255, 255, 255, 0.5);
}

.investigator-row {
  padding: 12px;
  background: rgba(255, 255, 255, 0.07);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 10px;
  display: flex;
  gap: 12px;
  align-items: flex-start;

  /* The one seat the table is waiting on you for. Same accent the deck list
     uses for a selected deck, so "this is the thing to act on" reads the same
     way at both levels. */
  &.investigator-row--choosing {
    background: rgba(110, 134, 64, 0.10);
    border-color: rgba(110, 134, 64, 0.40);
  }

  & :deep(.choices) {
    margin: 0;
    padding: 0;
  }
  & :deep(form) {
    margin: 0;
    height: fit-content;
  }

  .question {
    flex: 1;
    & :deep(.modal-contents) {
      border-radius: 5px;
      form {
        width: 100%;
        align-items: flex-start;
        display: flex;
        flex-direction: column;
        gap: 15px;
        label {
          text-transform: uppercase;
          margin-right: 15px;
        }
        button {
          width: 100%;
          margin: 0;
        }
      }
    }
    /* The amount panel's in-game mauve fights the coloured trauma fields, but a
       flat black wash leaves the purple submit stranded on blue-grey. A faint
       violet cast stays dark enough for the red and blue fields to read while
       giving the button a ground it belongs to. */
    & :deep(.amount-contents) {
      background: rgba(38, 28, 47, 0.45);
      border: 1px solid rgba(255, 255, 255, 0.09);

      /* In game the submit bleeds edge to edge, so the form carries the side
         padding and none at the bottom. Here it is an ordinary button sitting
         under the fields, which wants the panel padded evenly instead. */
      .amount-form {
        padding: 16px;
      }

      /* The in-game #3f2f48 lands at the same lightness as this panel, so it
         read as a smudge rather than a button. Same hue family, lifted clear of
         the ground; white on it is 5.8:1. */
      .amount-submit {
        transform: none;
        border-radius: 6px;
        background: #7e4f9e;
      }

      .amount-submit:hover:not([disabled]) {
        background: #8d5bb0;
      }

      /* The global disabled grey is !important, and a flat #999 slab is the
         first thing this prompt shows (both fields start at 0). Mute the purple
         instead of replacing it. */
      .amount-submit[disabled] {
        background-color: rgba(126, 79, 158, 0.38) !important;
        color: rgba(255, 255, 255, 0.6);
      }
    }
  }
}

.seated {
  width: 100px;
  flex-shrink: 0;
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.stat-chip {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 5px;
  padding: 5px 10px;
  background: rgba(0, 0, 0, 0.25);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 6px;
  font-size: 0.9em;
  font-weight: 700;
}

.stat-chip .icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}

.stat-health .icon { color: #f88; }
.stat-sanity .icon { color: #8af; }

.seated-stats {
  display: flex;
  gap: 6px;

  .stat-chip {
    flex: 1;
    padding: 5px 0;
  }
}

.seated-name {
  margin: 0;
  font-size: 0.7em;
  line-height: 1.3;
  text-align: center;
  color: rgba(255, 255, 255, 0.55);
}

/* A seat that is done: same height as one still waiting, so the row being
   acted on is the only tall thing on the page. */
.seated-summary {
  display: flex;
  align-items: center;
  gap: 12px;
  width: 100%;
  min-width: 0;
}

.seated-summary-portrait {
  width: 40px;
  height: 58px;
  flex-shrink: 0;
  object-fit: cover;
  object-position: top center;
  border-radius: 4px;
  box-shadow: 1px 1px 5px rgba(0, 0, 0, 0.45);
}

.seated-summary-text {
  display: flex;
  flex-direction: column;
  gap: 2px;
  min-width: 0;
}

.seated-summary-name {
  font-size: 0.94em;
  font-weight: 600;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.seated-summary-stats {
  display: flex;
  gap: 6px;
  margin-left: auto;
  flex-shrink: 0;
}

.portrait {
  width: 100px;
  border-radius: 5px;
  flex-shrink: 0;
  img {
    width: 100%;
    border-radius: 5px;
    box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  }
}

.portrait-empty {
  width: 100px;
  height: 155px;
  border-radius: 5px;
  flex-shrink: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  background: rgba(100, 100, 100, 0.3);
  display: flex;
  align-items: center;
  justify-content: center;
  img {
    width: 80%;
    opacity: 0.6;
  }
}

.deck-main {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.deck-requirements-card {
  padding: 10px 12px;
  border-radius: 8px;
  border: 1px solid rgba(255, 211, 112, 0.18);
  background: rgba(95, 65, 10, 0.24);
}

.deck-requirements-title {
  color: rgba(255, 255, 255, 0.78);
  font-size: 0.72em;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  margin-bottom: 8px;
}

.deck-requirements-body {
  display: flex;
  align-items: flex-end;
  justify-content: space-between;
  gap: 14px;
}

.valid-filter {
  width: auto;
  min-width: 150px;
  padding: 9px 13px;
  font-size: 0.74em;
  font-weight: 800;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  white-space: nowrap;
  color: rgba(255, 240, 196, 0.98);
  background: rgba(143, 98, 22, 0.62);
  border: 1px solid rgba(255, 211, 112, 0.34);
  border-radius: 6px;
  cursor: pointer;
  box-shadow: 0 4px 12px rgba(0,0,0,0.24);
  transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease, border-color 160ms ease;

  &:hover {
    transform: translateY(-1px);
    background: rgba(162, 112, 28, 0.78);
    border-color: rgba(255, 211, 112, 0.48);
    box-shadow: 0 7px 18px rgba(0,0,0,0.32);
  }

  &.active {
    background: rgba(178, 126, 36, 0.86);
    border-color: rgba(255, 211, 112, 0.58);
  }
}

.deck-requirements {
  flex: 1;
  margin: 0;
  padding-left: 18px;
  color: rgba(255, 226, 154, 0.95);
  line-height: 1.35;
  font-size: 0.82em;
}

/* Tab buttons — segmented control */
.deck-tabs {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  gap: 3px;
  padding: 3px;
  background: rgba(0,0,0,0.30);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 8px;

  button {
    height: 36px;
    border-radius: 6px;
    border: none;
    background: transparent;
    color: rgba(255,255,255,0.50);
    letter-spacing: 0.06em;
    text-transform: uppercase;
    font-size: 0.74em;
    cursor: pointer;
    transition: background 160ms ease, color 120ms ease, box-shadow 160ms ease;
    outline: none;

    &:hover:not(:disabled):not(.current) {
      background: rgba(255,255,255,0.06);
      color: rgba(255,255,255,0.80);
    }

    &.current {
      background: rgba(110, 134, 64, 0.88);
      color: white;
      box-shadow: 0 1px 4px rgba(0,0,0,0.35);
    }

    &:disabled {
      opacity: 0.30;
      cursor: not-allowed;
    }
  }
}

/* Deck picker (UseExistingDeck) */
.deck-picker {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.deck-list {
  display: flex;
  flex-direction: column;
  gap: 5px;
  max-height: calc(100dvh - 380px);
  min-height: 120px;
  overflow-y: auto;
  scrollbar-width: thin;
  scrollbar-color: rgba(255,255,255,0.15) transparent;
}

.deck-list-empty {
  padding: 24px;
  text-align: center;
  color: var(--button);
  font-size: 0.85em;
}

.deck-item {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 12px;
  padding: 10px 12px;
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.06);
  border-left: 3px solid transparent;
  border-radius: 6px;
  cursor: pointer;
  transition: background 0.12s, border-color 0.12s;
  color: #e0e0e0;

  &:hover { background: rgba(255,255,255,0.08); }

  &.guardian { border-left-color: var(--guardian-dark); &:hover { background: var(--guardian-extra-dark); } }
  &.seeker   { border-left-color: var(--seeker-dark);   &:hover { background: var(--seeker-extra-dark); } }
  &.rogue    { border-left-color: var(--rogue-dark);    &:hover { background: var(--rogue-extra-dark); } }
  &.mystic   { border-left-color: var(--mystic-dark);   &:hover { background: var(--mystic-extra-dark); } }
  &.survivor { border-left-color: var(--survivor-dark); &:hover { background: var(--survivor-extra-dark); } }
  &.neutral  { border-left-color: var(--neutral-dark);  &:hover { background: var(--neutral-extra-dark); } }

  /* Dim the row's own colors rather than filtering the subtree, which would
     also wash out the selection border and any nested panel. */
  &.deck-item--used {
    color: rgba(224, 224, 224, 0.45);
    .deck-item-portrait { opacity: 0.35; }
    .deck-item-use { opacity: 0.35; cursor: not-allowed; }
  }

  &.selected {
    border-color: rgba(110, 134, 64, 0.4);
    border-left-color: rgba(110, 134, 64, 0.9);
    background: rgba(110, 134, 64, 0.10);
  }

  &.has-error {
    border-color: rgba(200, 50, 50, 0.5);
    border-left-color: rgba(200, 50, 50, 0.9);
    background: rgba(160, 25, 25, 0.15);
  }
}

.deck-item-portrait {
  width: 60px;
  border-radius: 4px;
  flex-shrink: 0;
  box-shadow: 1px 1px 5px rgba(0,0,0,0.5);
}

.deck-item-info {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.deck-item-name {
  font-size: 0.94em;
  font-weight: 600;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.deck-item-taboo {
  font-size: 0.72em;
  font-weight: 600;
  color: #c8a96e;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.deck-item-overlaid {
  color: var(--spooky-green);
  font-size: 0.72em;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
}

.deck-item-error {
  font-size: 0.75em;
  color: #ff8080;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.deck-item-overlay,
.deck-item-use,
.deck-item-weakness-button {
  flex-shrink: 0;
  /* The global button rule pads 1px 11px, which leaves a fixed-width icon button
     no content box at all -- the icon then has zero width and Font Awesome paints
     its path at full 512px scale over the rows below. */
  padding: 0;
  width: 34px;
  height: 34px;
  border-radius: 5px;
  border: 1px solid rgba(255,255,255,0.10);
  color: white;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.85em;
  transition: background 150ms ease, transform 120ms ease, box-shadow 150ms ease;
  outline: none;

  &:hover {
    transform: scale(1.08);
    box-shadow: 0 4px 12px rgba(0,0,0,0.35);
  }

  &:active { transform: scale(1.0); }
}

.deck-item-use {
  background: rgba(110, 134, 64, 0.85);

  &:hover {
    background: rgba(110, 134, 64, 1);
  }
}

/* Lit only on the deck the pending overlay was built for, so it is clear which
   row it belongs to. */
.deck-item-overlay.active {
  background: rgba(235, 235, 235, 0.30);
  color: white;
}

.deck-item-weakness-button {
  width: 24px;
  height: 24px;
  font-size: 0.68em;
  background: rgba(235, 235, 235, 0.18);
  color: rgba(255, 255, 255, 0.86);
  backdrop-filter: blur(4px);

  &:hover,
  &.active {
    background: rgba(235, 235, 235, 0.30);
    color: white;
  }
}

/* Load New Deck layout: portrait left, form right */
.load-deck-layout {
  display: flex;
  gap: 12px;
  align-items: flex-start;
}

.load-deck-portrait {
  flex-shrink: 0;
}

.load-deck-content {
  flex: 1;
  min-width: 0;
}

/* UnsavedDeck form */
.deck-form {
  display: flex;
  flex-direction: column;
  gap: 10px;

  p {
    margin: 0;
    padding: 0;
  }

  p.unsaved-deck-name {
    color: #e0e0e0;
    padding: 12px 16px;
    background: rgba(255,255,255,0.05);
    border: 1px solid rgba(255,255,255,0.10);
    border-radius: 6px;
    text-align: center;
    font-weight: 600;
    letter-spacing: 0.04em;
    font-size: 0.92em;
  }

  p.deck-form-error {
    color: #ff8080;
    padding: 10px 12px;
    background: rgba(160, 25, 25, 0.15);
    border: 1px solid rgba(200, 50, 50, 0.5);
    border-radius: 6px;
    font-size: 0.82em;
    font-weight: 700;
    letter-spacing: 0.04em;
    text-transform: uppercase;
  }
}

.weakness-pool-panel {
  margin-top: 10px;
  border: 1px solid rgba(255,255,255,0.09);
  border-radius: 8px;
  background: rgba(0,0,0,0.16);
  overflow: hidden;
}

.deck-item-weakness-pool {
  flex: 0 0 100%;
  margin-top: 0;
  cursor: default;
  padding: 0 12px 12px;
}

/* OverlayEditor is also used in the deck page and campaign roster. Keep its
 * contents off the panel edge here, where the heading/help have their own
 * padding. */
.deck-item-weakness-pool :deep(.overlay-editor) {
  padding: 0;
}

.weakness-pool-toggle {
  width: 100%;
  border: 0;
  background: transparent;
  cursor: pointer;
  padding: 10px 12px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  color: rgba(255,255,255,0.86);
  font-size: 0.78em;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

.weakness-pool-toggle:hover {
  background: rgba(255,255,255,0.05);
}

.weakness-pool-heading {
  padding: 12px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  color: rgba(255,255,255,0.86);
  font-size: 0.78em;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

.weakness-pool-summary {
  color: rgba(255,255,255,0.55);
  font-size: 0.9em;
  font-weight: 600;
  text-align: right;
  text-transform: none;
  letter-spacing: 0;
}

.weakness-pool-help,
.deck-form p.weakness-pool-help {
  margin: 0;
  padding: 0 12px 12px;
  color: rgba(255,255,255,0.6);
  font-size: 0.82em;
}

.weakness-pool-actions {
  display: flex;
  gap: 8px;
  padding: 0 12px 10px;
}

.weakness-pool-actions button {
  border: 1px solid rgba(255,255,255,0.10);
  border-radius: 999px;
  background: rgba(255,255,255,0.07);
  color: rgba(255,255,255,0.78);
  padding: 5px 10px;
  font-size: 0.76em;
  cursor: pointer;
}

.weakness-pool-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
  gap: 6px;
  padding: 0 12px 12px;
}

.weakness-pool-option {
  display: flex;
  align-items: center;
  gap: 8px;
  color: rgba(255,255,255,0.82);
  font-size: 0.84em;
  padding: 5px 6px;
  border-radius: 5px;
  background: rgba(255,255,255,0.04);
}

.weakness-pool-option input {
  accent-color: rgb(110, 134, 64);
}

/* Primary action button */
.primary-action {
  width: 100%;
  height: 48px;
  border-radius: 5px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(110, 134, 64, 0.95);
  color: white;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  font-size: 0.88em;
  cursor: pointer;
  box-shadow: 0 5px 18px rgba(0,0,0,0.3);
  transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease;
  outline: none;

  &:hover:not(:disabled) {
    transform: translateY(-1px);
    background: rgba(110, 134, 64, 1);
    box-shadow: 0 10px 28px rgba(0,0,0,0.4);
  }

  &:active:not(:disabled) {
    transform: translateY(0);
  }

  &:disabled {
    opacity: 0.55;
    cursor: not-allowed;
    box-shadow: none;
    transform: none;
  }
}

/* Taboo shown on chosen investigator rows */
.taboo-list {
  color: #A8A749;
  font-size: 0.78em;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  padding: 4px 0;
}
</style>
