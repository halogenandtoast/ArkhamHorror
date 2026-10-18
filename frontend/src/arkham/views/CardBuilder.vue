<script lang="ts" setup>
/* The card builder: your library on the left, the card you are working on to
 * the right. Cards live against your account, so they outlive any one game.
 *
 * In a game you only pick from this library; building and editing happen here,
 * where there is room for it. */
import { computed, nextTick, onMounted, onUnmounted, reactive, ref, watch } from 'vue'
import { onBeforeRouteLeave, useRoute, useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import CustomCardForm from '@/arkham/components/debug/CustomCardForm.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import CardSetStrip from '@/arkham/components/CardSetStrip.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'
import Prompt from '@/components/Prompt.vue'
import { cardKind, stripCardCodePrefix } from '@/arkham/customCards'
import {
  mintCustomCardCode,
  renderCardPlaceholder,
  summarizeSignatures,
  type CustomCard,
  type SignatureSummary,
} from '@/arkham/customCards'
import { isBadLink, setLinkLabel } from '@/arkham/setLink'
import * as Api from '@/arkham/api'
import CustomCardsPage from '@/arkham/components/CustomCardsPage.vue'
import CardFilters from '@/arkham/components/CardFilters.vue'
import {
  chosenFacetCount,
  clearFilter,
  emptyCardFilter,
  filterCards,
  filterIsEmpty,
} from '@/arkham/customCardFilters'
import MetaChip from '@/arkham/components/MetaChip.vue'
import SetPreview from '@/arkham/components/SetPreview.vue'
import { useUserStore } from '@/stores/user'
import {
  createSet,
  exportCards,
  canExport,
  canUnlock,
  isAwaitingReview,
  isFromShare,
  isListed,
  isLocked,
  isShared,
  isSubscribed,
  publishSet,
  syncSet,
  updateAvailable,
  wasDenied,
  importSet,
  libraryCard,
  byPrintedNumber,
  libraryCards,
  libraryLoaded,
  librarySet,
  librarySets,
  loadLibrary,
  removeFromLibrary,
  removeSet,
  unlockSet,
  updateSet,
  saveToLibrary,
  setCards,
  type LibraryCard,
  type LibrarySet,
} from '@/arkham/customCardLibrary'
import ExpansionDetails, { type ExpansionDraft } from '@/arkham/components/debug/ExpansionDetails.vue'
import SetTextEditor from '@/arkham/components/debug/SetTextEditor.vue'
import RawCardEditor from '@/arkham/components/debug/RawCardEditor.vue'
import { importChanges, type CardChange } from '@/arkham/customCardDiff'

const { t, te } = useI18n()
const K = 'customCardSets.'

const form = ref<InstanceType<typeof CustomCardForm> | null>(null)
const editingCode = ref<string | null>(null)

/* Unsaved work. The baseline is what the editor looked like the last time it
 * agreed with the library -- loading a card, starting a new one, or saving --
 * and anything typed after that makes the two differ. Kept as a string so the
 * comparison is a comparison and not a deep walk on every keystroke. */
const baseline = ref<string | null>(null)
const dirty = computed(() => {
  const current = form.value?.snapshot()
  return current !== undefined && baseline.value !== null && current !== baseline.value
})

function markClean() {
  baseline.value = form.value?.snapshot() ?? null
}

/* Asking before work is lost. Resolved by the modal, so a caller can await the
 * answer the same way it would await a navigation. */
const unsavedAsk = ref<((keepGoing: boolean) => void) | null>(null)

function confirmDiscard(): Promise<boolean> {
  if (!dirty.value) return Promise.resolve(true)
  return new Promise((resolve) => {
    unsavedAsk.value = (keepGoing) => {
      unsavedAsk.value = null
      resolve(keepGoing)
    }
  })
}
const selected = ref<string[]>([])

/* Whether the panel is picking cards rather than opening them. A checkbox on
 * every row is a column of chrome for something done rarely -- selecting is
 * only ever for exporting -- so the rows stay cards until you ask. */
const selecting = ref(false)

function toggleSelecting() {
  selecting.value = !selecting.value
  if (!selecting.value) selected.value = []
}
const busy = ref(false)
const libraryCollapsed = ref(false)
const status = ref<string | null>(null)
const error = ref<string | null>(null)

/* Which set new cards are written into. Remembered across visits because it is
 * the thing you are working on, not a filter you re-pick every time. */
const ACTIVE_SET_KEY = 'arkham:card-builder:active-set'
const activeSetId = ref<string | null>(null)
const newSetName = ref('')
/* Renaming, redescribing and relinking are one form: they are what a set says
 * about itself, and asking for them in three places means three round trips to
 * change what reads as one thing. */
const renamingSetId = ref<string | null>(null)
const renameDraft = ref('')
const describeDraft = ref('')
const linkDraft = ref('')
/* What marking the set an investigator expansion unlocks: its symbol, its own
 * tokens, its own keywords. Part of the same form, and so the same request: all
 * of it is what the set says about itself. */
const expansionDraft = ref<ExpansionDraft>({
  investigatorExpansion: false,
  icon: null,
  tokens: [],
  keywords: [],
})

const route = useRoute()
const router = useRouter()

/* An admin's publish is listed outright rather than queued -- they are the person
 * review would wait for. The server decides that on its own; this only picks the
 * wording, so a stale flag cannot list anything it should not. */
const userStore = useUserStore()

/* Which set a submission is being written for, the note to send with it, and
 * whether the author wants to be emailed the decision. Held here rather than
 * prompted for, because "what changed" wants a text field and a confirm dialog
 * has none.
 *
 * Notifying defaults to on: somebody who has just asked a person to look at their
 * work wants to hear back, and it is one click to say otherwise. It is not asked
 * of an admin, who is going to be the one deciding. */
const publishingSetId = ref<string | null>(null)
const publishNote = ref('')
const publishNotify = ref(true)

function startPublish(set: LibrarySet) {
  publishingSetId.value = set.id
  publishNote.value = ''
  publishNotify.value = true
  // Both answer "what do I do with this set", and open together they are a wall.
  sharingSetId.value = null
  status.value = null
  error.value = null
}

/* What the shelf says about a set -- the blurb and the link -- belongs to the
 * set, and is written where the set's name is written. Publishing shows it and
 * hands you that form rather than asking for it a second time: two fields that
 * fill the same two columns are two chances for them to disagree.
 *
 * Which set's publish form it was opened from, so closing the details comes
 * back to it rather than to the list: the blurb was being edited for the sake
 * of the submission, and the note typed into it is still there. */
const detailsForPublish = ref<string | null>(null)

function editDetails(set: LibrarySet) {
  publishingSetId.value = null
  startRename(set)
  detailsForPublish.value = set.id
}

function leaveDetails(set: LibrarySet) {
  renamingSetId.value = null
  if (detailsForPublish.value !== set.id) return
  detailsForPublish.value = null
  publishingSetId.value = set.id
}

/* For an admin this lists the set; for anybody else it submits it, and the
 * version goes into the review queue with nothing about the marketplace changed
 * until somebody acts on it. The message says which happened, because
 * "Published" would otherwise be a lie the author only finds out about later. */
async function commitPublish(set: LibrarySet) {
  const note = publishNote.value.trim()
  const notify = publishNotify.value
  publishingSetId.value = null
  error.value = null
  try {
    const submitted = await publishSet(set.id, note || null, notify)
    status.value = t(`${K}${userStore.isAdmin ? 'published' : 'submitted'}`, {
      name: set.name,
      version: submitted.pendingVersion ?? submitted.latestVersion,
    })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}${userStore.isAdmin ? 'listFailed' : 'publishFailed'}`)
  }
}

/* What the marketplace button does, which for an admin is list it rather than ask
 * for it to be listed. */
function publishTitle(set: LibrarySet): string {
  return t(`${K}${userStore.isAdmin ? 'publishTitle' : 'submitTitle'}`, { name: set.name })
}

/* Where a set stands with the marketplace, for the states that need a sentence:
 * waiting on somebody, or turned down and here is why. A set that is simply
 * listed is good news and gets a chip, not a paragraph. Read off the set rather
 * than refetched: the library is reloaded after every submission. */
function reviewLine(set: LibrarySet): string | null {
  if (isAwaitingReview(set)) {
    return isListed(set)
      ? t(`${K}reviewPendingUpdate`, { version: set.submittedVersion, live: set.approvedVersion })
      : t(`${K}reviewPending`, { version: set.submittedVersion })
  }
  if (wasDenied(set)) return t(`${K}reviewDenied`, { version: set.submittedVersion })
  // Listed and settled says itself in the chip beside the card count.
  return null
}

/* Pull the newest published version into a subscribed set. Its cards are
 * replaced outright, which is what makes it the published set again. */
async function update(set: LibrarySet) {
  error.value = null
  status.value = null
  try {
    const published = await syncSet(set.id)
    status.value = t(`${K}updated`, { name: set.name, version: published.latestVersion })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}updateFailed`)
  }
}

/* ------------------------------------------------------------------- lock ---

   A set taken from a listing or a link follows it, which means it stays what was
   published or shared: the server refuses every edit, and the controls that
   would try are not shown. This is the one button that changes that. */
async function unlock(set: LibrarySet) {
  status.value = null
  error.value = null
  try {
    await unlockSet(set.id)
    status.value = t(`${K}unlocked`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}unlockFailed`)
  }
}

/* ------------------------------------------------------------------ share ---

   Handing an unpublished set to a few people. The marketplace is the other way
   to do it and the wrong one for a set that is not finished: it is public,
   reviewed, and versioned, and a playtest wants the people trying the cards to
   have whatever changed this afternoon.

   The panel is opened per set, like the publish form, and the two are mutually
   exclusive: they are both "what do I do with this set", and open together they
   would be a wall. */
const sharingSetId = ref<string | null>(null)
const share = ref<Api.CardSetShare | null>(null)
const shareBusy = ref(false)
const copied = ref(false)
/* Both destructive in their own way -- one kills the link for everybody, the
 * other for one person -- so each asks once, inline, rather than in a dialog
 * over the top of the list it is about. */
const confirmingStop = ref(false)
const confirmingRotate = ref(false)

const shareLink = computed(() => (share.value ? Api.shareUrl(share.value.token) : null))

function resetShare() {
  share.value = null
  copied.value = false
  confirmingStop.value = false
  confirmingRotate.value = false
}

/* Opens on what is there: a set already shared loads its link and its people, a
 * set that is not shows the one button that starts it. A 404 is the answer to
 * "is this shared", not a failure. */
async function openShare(set: LibrarySet) {
  publishingSetId.value = null
  status.value = null
  error.value = null
  if (sharingSetId.value === set.id) {
    sharingSetId.value = null
    resetShare()
    return
  }
  sharingSetId.value = set.id
  resetShare()
  if (!set.shareToken) return
  shareBusy.value = true
  try {
    share.value = await Api.fetchCardSetShare(set.id)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}shareLoadFailed`)
  } finally {
    shareBusy.value = false
  }
}

async function startShare(set: LibrarySet) {
  shareBusy.value = true
  error.value = null
  try {
    share.value = await Api.shareCardSet(set.id)
    await loadLibrary(true)
    status.value = t(`${K}shareStarted`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}shareFailed`)
  } finally {
    shareBusy.value = false
  }
}

/* A new link, which makes the old one worthless -- the way to deal with one that
 * got somewhere it should not have. The people stay: they are the record of who
 * took the set, which is still true of them, and anyone who should not have it
 * is removed by themselves. */
async function rotateShare(set: LibrarySet) {
  confirmingRotate.value = false
  shareBusy.value = true
  error.value = null
  try {
    share.value = await Api.shareCardSet(set.id, true)
    copied.value = false
    status.value = t(`${K}shareRotated`)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}shareFailed`)
  } finally {
    shareBusy.value = false
  }
}

/* Stop sharing. The link dies and the list of who had it goes with it; what
 * anybody already took stays in their own library, which is what the warning
 * above the button says. */
async function stopShare(set: LibrarySet) {
  confirmingStop.value = false
  shareBusy.value = true
  error.value = null
  try {
    await Api.unshareCardSet(set.id)
    resetShare()
    sharingSetId.value = null
    await loadLibrary(true)
    status.value = t(`${K}shareStopped`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}shareFailed`)
  } finally {
    shareBusy.value = false
  }
}

/* One person, without disturbing the link everyone else holds. Nothing of
 * theirs is taken away -- the cards they took are theirs -- all this stops is
 * reading the set and taking it again. */
async function setAccess(set: LibrarySet, access: Api.CardSetShareAccess, revoked: boolean) {
  shareBusy.value = true
  error.value = null
  try {
    share.value = await Api.setCardSetShareAccess(set.id, access.id, revoked)
    await loadLibrary(true)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}shareFailed`)
  } finally {
    shareBusy.value = false
  }
}

async function copyLink() {
  if (!shareLink.value) return
  try {
    await navigator.clipboard.writeText(shareLink.value)
    copied.value = true
  } catch {
    copied.value = false
  }
}

/* Where one person stands with the set, in the words the author needs: they have
 * it, they have an old one, or they have only looked. */
function accessLine(access: Api.CardSetShareAccess): string {
  if (access.revoked) return t(`${K}shareRemoved`)
  if (!access.takenAt) return t(`${K}shareLookedOnly`)
  const date = new Date(access.takenAt).toLocaleDateString()
  return access.behind ? t(`${K}shareHasOlder`, { date }) : t(`${K}shareHasCurrent`, { date })
}

/* ?card=<code> opens the builder on that card: a deep link from a game ("Edit
 * custom card" on an asset), and the way one card links to another — a
 * signature to the investigator whose it is. Watched rather than read once, so
 * a link followed while already here still lands. */
async function openFromRoute() {
  await loadLibrary()
  /* The url names the set, so a link to one opens it and a reload stays on it.
   * A set that is not yours (or no longer exists) falls through to the list
   * rather than to somebody else's set. */
  const routed = typeof route.params.setId === 'string' ? route.params.setId : null
  if (routed && sets.value.some((s) => s.id === routed)) {
    if (activeSetId.value !== routed) chooseActiveSet(routed)
    browsingSets.value = false
  } else if (routed) {
    browsingSets.value = true
  }
  ensureActiveSet()
  const wanted = route.query.card
  if (typeof wanted !== 'string') return
  // A code travels with the 'c' the engine prepends or without it, and the
  // library holds whichever form was saved, so match on the bare form.
  const code = stripCardCodePrefix(wanted)
  if (editingCode.value && stripCardCodePrefix(editingCode.value) === code) return
  const card = cards.value.find((c) => stripCardCodePrefix(c.def.cardCode) === code)
  if (card) await edit(card)
}

onMounted(async () => {
  await openFromRoute()
  // Nothing has been typed yet, whether a card opened or the editor is blank.
  if (baseline.value === null) markClean()
})
watch(() => [route.params.setId, route.query.card], openFromRoute)

/* Leaving the builder entirely. Navigating within it -- which is what opening a
 * card does, since the url names the card -- is not leaving: those paths ask on
 * their own, before they replace what is in the editor. */
onBeforeRouteLeave(async (to) =>
  to.name === 'CardBuilder' || to.name === 'CardBuilderSet' ? true : await confirmDiscard(),
)

/* And leaving the site, where the browser asks in its own words and all we can
 * do is say that there is something to lose. */
function warnOnUnload(event: BeforeUnloadEvent) {
  if (!dirty.value) return
  event.preventDefault()
  event.returnValue = ''
}

/* The head is only separated from the card once it is over it. A line drawn
 * while it is still at the top of the panel would be a line under a heading,
 * which is a different thing to say. */
const headEl = ref<HTMLElement | null>(null)
const headStuck = ref(false)

/* The page scrolls inside a box of its own rather than the window, so that box
 * is what the head is measured against. */
function scrollParent(el: HTMLElement): HTMLElement | null {
  for (let at = el.parentElement; at; at = at.parentElement) {
    if (/auto|scroll|overlay/.test(getComputedStyle(at).overflowY)) return at
  }
  return null
}

/* Watched rather than observed once at mount: the editor arrives with the set,
 * which is fetched, so at mount there is no head to measure yet. */
let headWatcher: IntersectionObserver | null = null

watch(headEl, (el) => {
  headWatcher?.disconnect()
  headWatcher = null
  headStuck.value = false
  if (!el) return
  // A pixel of the top is cut off what it is measured against, so the head
  // stops fitting exactly when it comes to rest against that top.
  headWatcher = new IntersectionObserver(
    ([entry]) => (headStuck.value = entry.intersectionRatio < 1),
    { root: scrollParent(el), rootMargin: '-1px 0px 0px 0px', threshold: 1 },
  )
  headWatcher.observe(el)
})

onUnmounted(() => headWatcher?.disconnect())

onMounted(() => window.addEventListener('beforeunload', warnOnUnload))
onUnmounted(() => window.removeEventListener('beforeunload', warnOnUnload))

const cards = computed(() => libraryCards())
const sets = computed(() => librarySets())
const activeSet = computed(() => librarySet(activeSetId.value))

/* The set open in the editor, when it is one that follows something. Drives the
 * editor pane: no new cards, no saving, no deleting, and the form itself inside
 * a disabled fieldset, so its fields read rather than invite. */
const activeSetLocked = computed(() => !!activeSet.value && isLocked(activeSet.value))

/* The two things that depend on where a locked set came from. A copy of
 * something published can be taken into the editor and exported like any set of
 * yours; one taken from a share link is the author's unpublished work, so it can
 * be neither. */
const activeSetUnlockable = computed(() => !!activeSet.value && canUnlock(activeSet.value))
const activeSetExportable = computed(() => !!activeSet.value && canExport(activeSet.value))

/* An expansion leaves as an expansion or not at all.
 *
 * Its cards carry a copy of what the set declares -- the tokens they place on
 * themselves, the keywords they print -- because that copy is the only form
 * those declarations reach a running game in. A card taken out on its own
 * would have to carry the whole box with it to say what its keyword means, so
 * it does not go out on its own: the set is the unit, and the file carries the
 * declarations once at the top instead of on every card in it. */
const wholeSetOnly = computed(() => !!activeSet.value?.investigatorExpansion)

/* What the open set declares, for the card form: the keywords a card here can
 * print, and the tokens one of them can be marked with. Empty for a set that is
 * not an investigator expansion, which is what keeps the form from offering a
 * keyword section to a set that has none. */
const openSetDeclaration = computed(() => ({
  keywords: activeSet.value?.keywords ?? [],
  tokens: activeSet.value?.tokens ?? [],
}))

/* What the set declares, in a line. A count rather than the lists themselves:
 * the button is a way in, and a panel about cards should not grow a section per
 * kind of thing an expansion can hold. */
const declarationSummary = computed(() => {
  const set = activeSet.value
  if (!set?.investigatorExpansion) return t(`${K}notAnExpansion`)
  const parts = [
    t(`${K}tokenCount`, set.tokens?.length ?? 0),
    t(`${K}keywordCount`, set.keywords?.length ?? 0),
  ]
  return parts.join(', ')
})

/* Editing what the open set declares, without leaving it. The sets page can do
 * this too, as part of the whole details form; this is the same edit reached
 * from where the cards that use it are being written. */
const editingDeclaration = ref(false)

/* Open everything the set declares, in the main panel. */
function startDeclaration(set: LibrarySet) {
  startRename(set)
  // `startRename` opens the details form on the sets page; only the expansion
  // half is wanted here, so that is closed again and the draft kept.
  renamingSetId.value = null
  editingDeclaration.value = true
}

async function saveDeclaration(set: LibrarySet) {
  const expansion = expansionDraft.value
  editingDeclaration.value = false
  error.value = null
  try {
    await updateSet(set.id, {
      name: set.name,
      investigatorExpansion: expansion.investigatorExpansion,
      icon: expansion.icon,
      tokens: expansion.tokens,
      keywords: expansion.keywords,
    })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}setRenameFailed`)
  }
}

const inPrintedOrder = (setId: string) => setCards(setId).sort(byPrintedNumber)

/* The set being worked on, and the cards in it. A card is built into a set, so
 * the builder shows one at a time rather than the whole library at once. */
const activeCards = computed(() =>
  activeSetId.value ? inPrintedOrder(activeSetId.value) : [],
)

/* What the panel actually lists. Everything that acts on "the cards" -- the
 * count, selecting them all, exporting the selection -- acts on these, so a
 * filtered panel never quietly picks up a card it is not showing. */
const visibleCards = computed(() => filterCards(activeCards.value, filter))

/* Two pages, not one: your sets, or the editor for the set you opened. The
 * editor is a card at a time, so browsing sets there meant the panel and the
 * main area were showing different things. On its own page a set has room to
 * open up and show what is in it. */
const browsingSets = ref(true)
const inSet = computed(() => !browsingSets.value && !!activeSet.value)

/* Opening a set leaves the sets page for the editor, on a blank card: you came
 * here to build, and any card in the set is one click away in the panel. */
function openSet(id: string) {
  chooseActiveSet(id)
  browsingSets.value = false
  editingDeclaration.value = false
  startNew()
  router.push({ name: 'CardBuilderSet', params: { setId: id } })
}

/* Back to the list, and to the url that is the list. Pushed rather than
 * replaced: clicking into a set and out again is two steps, and the back button
 * should retrace them. */
function leaveSet() {
  browsingSets.value = true
  editingDeclaration.value = false
  router.push({ name: 'CardBuilder', query: {} })
}

/* Falls back to whatever set is first rather than leaving nothing selected: a
 * builder with sets in it should always be pointed at one of them. */
function chooseActiveSet(id: string | null) {
  activeSetId.value = id
  selected.value = []
  try {
    if (id) localStorage.setItem(ACTIVE_SET_KEY, id)
    else localStorage.removeItem(ACTIVE_SET_KEY)
  } catch {
    // A browser refusing storage is not a reason to fail to switch sets.
  }
}

function ensureActiveSet() {
  if (activeSetId.value && sets.value.some((s) => s.id === activeSetId.value)) return
  let remembered: string | null = null
  try {
    remembered = localStorage.getItem(ACTIVE_SET_KEY)
  } catch {
    remembered = null
  }
  const known = remembered && sets.value.some((s) => s.id === remembered) ? remembered : null
  chooseActiveSet(known ?? sets.value[0]?.id ?? null)
}

// ---------------------------------------------------------- browse & sort ---

/* One filter for both halves of the builder: the list of sets, and the panel
 * inside a set. Searching for a card on the sets page and then opening the set
 * it is in should not throw away what you typed to find it. */
const filter = reactive(emptyCardFilter())
const filtering = computed(() => !filterIsEmpty(filter))
const setOrder = ref<'name' | 'recent'>('name')
const setOrderOptions = computed(() => [
  { value: 'name' as const, label: t(`${K}orderAlphabetical`) },
  { value: 'recent' as const, label: t(`${K}orderRecent`) },
])

/* Saving a card does not touch its set's row -- that only moves on a rename or
 * an import -- so how recently a set was worked on is the newest card in it. */
const lastTouched = (set: LibrarySet) =>
  setCards(set.id).reduce((newest, c) => (c.updatedAt > newest ? c.updatedAt : newest), set.updatedAt)

/* The cards in a set that answer the filter; all of them when there is none. */
function matchingCards(setId: string) {
  return filterCards(inPrintedOrder(setId), filter)
}

/* A set's name only answers for the typed words. Once a chip is lit the
 * question is about cards -- "the locations" is not answered by a set called
 * Locations with none in it -- so the name stops standing in for them. */
function matchesSetName(set: LibrarySet) {
  const query = filter.query.trim().toLowerCase()
  if (!query || chosenFacetCount(filter) > 0) return false
  return set.name.toLowerCase().includes(query)
}

/* A set stays in the list when its own name matches or one of its cards does,
 * so searching for a card finds the set holding it. */
const visibleSets = computed(() => {
  const shown = filtering.value
    ? sets.value.filter((s) => matchesSetName(s) || matchingCards(s.id).length > 0)
    : [...sets.value]
  // `sets` is already alphabetical, so only the other order has to be applied.
  if (setOrder.value === 'recent') {
    return shown.sort((a, b) => lastTouched(b).localeCompare(lastTouched(a)))
  }
  return shown
})

const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)
const isSelected = (code: string) => selected.value.includes(code)

function toggleSelected(code: string) {
  const index = selected.value.indexOf(code)
  if (index === -1) selected.value.push(code)
  else selected.value.splice(index, 1)
}

const selectAll = () => (selected.value = visibleCards.value.map((c) => c.def.cardCode))
const clearSelection = () => (selected.value = [])

// -------------------------------------------------------------------- sets ---

async function addSet() {
  const name = newSetName.value.trim()
  if (!name) return
  error.value = null
  try {
    const set = await createSet(name)
    newSetName.value = ''
    openSet(set.id)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}setCreateFailed`)
  }
}

function startRename(set: LibrarySet) {
  renamingSetId.value = set.id
  detailsForPublish.value = null
  renameDraft.value = set.name
  describeDraft.value = set.description ?? ''
  linkDraft.value = set.url ?? ''
  expansionDraft.value = {
    investigatorExpansion: set.investigatorExpansion ?? false,
    icon: set.icon ?? null,
    // Copied rather than referenced: the editor replaces the whole draft on
    // every keystroke, and the set in the library must not change until saved.
    tokens: (set.tokens ?? []).map((token) => ({ ...token })),
    keywords: (set.keywords ?? []).map((keyword) => ({ ...keyword })),
  }
}

/* Only what actually changed is sent. The description and the link are left out
 * of the body when they are untouched, which is what keeps an older blurb or
 * link from being cleared; and a form closed without changing anything sends
 * nothing at all, so a set that follows a published one does not lose that for
 * a no-op rename.
 *
 * The server refuses a link that is not an http address, which is the one way
 * this form can fail on something other than the name. */
async function commitRename(set: LibrarySet) {
  const name = renameDraft.value.trim()
  const description = describeDraft.value.trim()
  const url = linkDraft.value.trim()
  const expansion = expansionDraft.value
  leaveDetails(set)
  if (!name) return
  const renamed = name !== set.name
  const redescribed = description !== (set.description ?? '')
  const relinked = url !== (set.url ?? '')
  /* Unticking the box leaves the tokens and the keywords where they are rather
   * than sending empty lists: somebody who ticks it again has not lost the work,
   * and a set that is not an expansion is not asked about either of them
   * anywhere. */
  const nowExpansion = expansion.investigatorExpansion
  const declared = nowExpansion
    ? { icon: expansion.icon, tokens: expansion.tokens, keywords: expansion.keywords }
    : {}
  const expansionChanged =
    nowExpansion !== (set.investigatorExpansion ?? false)
    || (nowExpansion && !sameDeclaration(set, expansion))
  if (!renamed && !redescribed && !relinked && !expansionChanged) return
  error.value = null
  try {
    await updateSet(set.id, {
      name,
      ...(redescribed ? { description: description || null } : {}),
      ...(relinked ? { url: url || null } : {}),
      ...(expansionChanged ? { investigatorExpansion: nowExpansion, ...declared } : {}),
    })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}${isBadLink(e) ? 'setLinkInvalid' : 'setRenameFailed'}`)
  }
}

/* Whether the draft says the same as the set. Compared as JSON rather than
 * field by field: these are small lists of plain records, and the alternative is
 * a deep-equality walk written out for two shapes that will grow. */
function sameDeclaration(set: LibrarySet, draft: ExpansionDraft): boolean {
  return (
    (set.icon ?? null) === draft.icon
    && JSON.stringify(set.tokens ?? []) === JSON.stringify(draft.tokens)
    && JSON.stringify(set.keywords ?? []) === JSON.stringify(draft.keywords)
  )
}


/* The whole point of a set: changing your mind about an import you just made is
 * one decision, so the count is spelled out rather than left to be discovered. */
/* Deleting takes the same modal a game does, rather than the browser's own
 * confirm: it is the one destructive thing on this page, and a set is a lot of
 * work to lose to a dialog you have already learned to click through. */
const deletingSet = ref<LibrarySet | null>(null)
const deletingCard = ref<CustomCard | null>(null)

/* The card whose def is open as JSON. Held as the library row rather than the
 * def, since saving one needs the set it belongs to and the art it keeps. */
const rawCard = ref<LibraryCard | null>(null)

/* The number printed on a card, which is what the panel is sorted by and so
 * the thing to read down the left edge of it. Written by hand, so it is a
 * string: "134", but also "12a" and "R3". */
const printedNumber = (card: LibraryCard) => {
  const number = card.def.meta?.number
  return typeof number === 'string' ? number.trim() : typeof number === 'number' ? String(number) : ''
}

/* Only a set that numbers its cards gets the column. A box does; a handful of
 * cards made one afternoon does not, and an empty gutter down the side of it
 * would be a column of nothing. */
const numbered = computed(() => visibleCards.value.some((card) => printedNumber(card)))

/* The row behind the open form, which is what the raw editor edits. A card
 * being written for the first time has no stored def to read. */
const editingRaw = computed(() => (editingCode.value ? (libraryCard(editingCode.value) ?? null) : null))

/* The card by name once it has one, since "Editing card" says the least of
 * anything on the page. Read off the form, so renaming one renames the head. */
const editingTitle = computed(() => (form.value?.cardTitle ?? '').trim())
const rawBusy = ref(false)

async function saveRaw(def: Record<string, any>) {
  const card = rawCard.value
  if (!card) return
  rawBusy.value = true
  error.value = null
  try {
    const saved = await saveToLibrary({ def: def as CustomCard['def'], art: card.art }, card.setId)
    rawCard.value = null
    // The form may be showing the card that just changed underneath it.
    if (editingCode.value === saved.def.cardCode) form.value?.loadCard(saved)
    status.value = t(`${K}rawSaved`, { name: saved.def.name.title })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}rawFailed`)
  } finally {
    rawBusy.value = false
  }
}

const deleteSetPrompt = computed(() => {
  const set = deletingSet.value
  if (!set) return ''
  const what = set.cardCount ? t(`${K}deleteSetCards`, set.cardCount) : t(`${K}deleteSetEmpty`)
  return t(`${K}confirmDeleteSet`, { name: set.name, what })
})

async function dropSet(set: LibrarySet) {
  deletingSet.value = null
  error.value = null
  try {
    const editing = editingCode.value
    await removeSet(set.id)
    if (activeSetId.value === set.id) chooseActiveSet(sets.value[0]?.id ?? null)
    if (editing && !libraryCard(editing)) startNew()
    status.value = t(`${K}setDeleted`, { name: set.name })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}setDeleteFailed`)
  }
}

async function edit(card: CustomCard) {
  if (!(await confirmDiscard())) return
  editingDeclaration.value = false
  editingCode.value = card.def.cardCode
  browsingSets.value = false
  status.value = null
  error.value = null
  /* Opening a card from elsewhere -- a game, or an investigator's signature --
   * switches to the set it lives in, so the library beside it is showing the
   * card that is open. */
  const owned = libraryCard(card.def.cardCode)
  if (owned && owned.setId !== activeSetId.value) chooseActiveSet(owned.setId)
  await nextTick()
  await form.value?.loadCard(card)
  markClean()
  syncRoute(card.def.cardCode)
}

async function startNew() {
  if (!(await confirmDiscard())) return
  // The declaration editor and the card form share the main panel, so reaching
  // for a card is also leaving the declaration.
  editingDeclaration.value = false
  editingCode.value = null
  status.value = null
  error.value = null
  form.value?.reset()
  markClean()
  syncRoute(null)
}

/* The url names the card being edited, so a refresh comes back to it and the
 * link is worth sharing. Replaced rather than pushed: opening one card after
 * another should not fill up the back button. */
function syncRoute(cardCode: string | null) {
  // Whichever page the card is on: a card opened inside a set keeps the set's
  // url, so the link carries both and a reload comes back to the same place.
  const where = activeSetId.value && !browsingSets.value
    ? { name: 'CardBuilderSet', params: { setId: activeSetId.value } }
    : { name: 'CardBuilder' }
  const current = route.query.card
  const sameSet = route.params.setId === (where.params?.setId ?? undefined)
  if (cardCode === null) {
    if (current === undefined && sameSet) return
    router.replace({ ...where, query: {} })
    return
  }
  if (
    sameSet
    && typeof current === 'string'
    && stripCardCodePrefix(current) === stripCardCodePrefix(cardCode)
  ) {
    return
  }
  router.replace({ ...where, query: { card: cardCode } })
}

async function save() {
  /* A card is written into a set, so there has to be one. Editing a card keeps
   * it in the set it is already in rather than moving it to whichever set
   * happens to be active. */
  const setId = (editingCode.value ? libraryCard(editingCode.value)?.setId : null) ?? activeSetId.value
  if (!setId) {
    error.value = t(`${K}needASet`)
    return
  }

  busy.value = true
  error.value = null
  status.value = null

  try {
    // Editing keeps the card's code, so the save replaces it everywhere rather
    // than leaving a second copy behind.
    const card = form.value?.buildCustomCard(editingCode.value ?? mintCustomCardCode())
    if (!card) return
    const saved = await saveToLibrary(card, setId)
    editingCode.value = saved.def.cardCode
    markClean()
    status.value = t(`${K}saved`)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}saveFailed`)
  } finally {
    busy.value = false
  }
}

async function remove(card: CustomCard) {
  deletingCard.value = null
  await removeFromLibrary(card.def.cardCode)
  selected.value = selected.value.filter((c) => c !== card.def.cardCode)
  if (editingCode.value === card.def.cardCode) startNew()
}

// ---------------------------------------------------------------- export ---

/* The set a card belongs to, which is what decides whether it may leave. Read
 * per card rather than from the open set, because the selection and the one-card
 * export both run from a panel that is only ever showing one set, and a wrong
 * answer here is somebody's unpublished work in a file. */
const exportable = (cards: CustomCard[]) =>
  cards.every((c) => {
    const set = librarySet(libraryCard(c.def.cardCode)?.setId ?? null)
    return !set || canExport(set)
  })

async function download(cards: CustomCard[], filename: string, set?: LibrarySet) {
  /* Somebody else's unpublished work is not yours to hand on. The buttons are
   * not rendered for one, so nothing here should reach this -- it is the rule
   * stated where the file is written rather than only where it is asked for.
   *
   * Not a guarantee, and not meant as one: the cards are in the library this
   * reads because playing with them needs them there. It is the affordance that
   * is withdrawn. */
  if ((set && !canExport(set)) || !exportable(cards)) return
  // The art is fetched and inlined, so this waits on the network.
  const blob = new Blob([JSON.stringify(await exportCards(cards, set), null, 2)], {
    type: 'application/json',
  })
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = filename
  link.click()
  URL.revokeObjectURL(url)
}

const slug = (text: string) => text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'card'

const exportOne = (card: CustomCard) => download([card], `${slug(card.def.name.title)}.arkhamcard.json`)

/* A set exports as a set, naming itself in the file, so importing it again --
 * here or on someone else's account -- lands as that set rather than as loose
 * cards needing somewhere to go. */
const exportSet = (set: LibrarySet) =>
  download(inPrintedOrder(set.id), `${slug(set.name)}.arkhamcard.json`, set)

/* With cards picked, the picked ones; with none, the whole set -- which is the
 * answer to "export" when nothing has been singled out, and the file somebody
 * syncing another library actually wants. A set export carries the set itself,
 * so it lands as that set rather than as loose cards needing somewhere to go. */
async function exportSelected() {
  const chosen = wholeSetOnly.value ? [] : activeCards.value.filter((c) => isSelected(c.def.cardCode))
  if (!chosen.length) {
    if (activeSet.value) await exportSet(activeSet.value)
    return
  }
  await download(
    chosen,
    chosen.length === 1 ? `${slug(chosen[0].def.name.title)}.arkhamcard.json` : `custom-cards-${chosen.length}.json`,
  )
}

/* How many investigator minis the last import had to cut for itself. Set by the
 * arkham.build path, cleared by every import, and only ever read to say so. */
const portraitsCut = ref(0)

/* A parsed file waiting on a yes. Held here rather than asked through the
 * browser's own confirm, which had room for one sentence: what matters about an
 * import is what is in the file, which set it lands on, and what that costs the
 * set already there. */
type PendingImport = {
  fileName: string
  name: string
  // Only ever something when the file said so; the server leaves the set's own
  // alone otherwise.
  description: string | null
  url: string | null
  sourceCode: string | null
  cards: CustomCard[]
  replacing: LibrarySet | null
  minisCut: number
  signatures: SignatureSummary | null
  /* A set to add these to, rather than a set to become.
   *
   * A file exported as a single card carries no set of its own, but its def still
   * remembers the one it came from, so importing it landed as a set named after
   * somebody else's library. Given a destination it is added to that instead, and
   * the set name on the card is restamped to match. */
  into: LibrarySet | null
}

const pendingImport = ref<PendingImport | null>(null)
const importPanel = ref<HTMLElement | null>(null)
const importPreview = ref<HTMLElement | null>(null)
const importExpanded = ref(false)
/* Whether the clipped strip is hiding anything. Measured rather than counted:
 * how many cards fit one row is the column's business, not ours. */
const importOverflows = ref(false)

const cancelImport = () => {
  pendingImport.value = null
  importExpanded.value = false
  importOverflows.value = false
}

watch([pendingImport, importExpanded], async () => {
  if (!pendingImport.value || importExpanded.value) return
  await nextTick()
  const strip = importPreview.value?.querySelector('.card-strip')
  importOverflows.value = !!strip && strip.scrollHeight > strip.clientHeight + 4
})

/* Types in descending order of how much of the file they are, so the first line
 * of the breakdown is what the set mostly is. */
const importBreakdown = computed(() => {
  const counts = new Map<string, number>()
  for (const card of pendingImport.value?.cards ?? []) {
    counts.set(card.def.cardType, (counts.get(card.def.cardType) ?? 0) + 1)
  }
  return [...counts.entries()]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .map(([cardType, count]) => ({
      cardType,
      count,
      label: te(`${K}types.${cardType}`) ? t(`${K}types.${cardType}`) : cardType,
    }))
})

/* Cards in the file that your library already holds somewhere else.
 *
 * A card belongs to one set and the row is keyed on its code, so writing it
 * into a second set moves it rather than copying it -- the set it came from is
 * left a card short, and nothing says so. The server refuses it; this is the
 * same answer given before the yes rather than after it, where it can name
 * every card at once and point at the set they are in. */
const importElsewhere = computed(() => {
  const incoming = pendingImport.value
  if (!incoming) return []
  const target = (incoming.into ?? incoming.replacing)?.id ?? null
  const found: { title: string; set: string }[] = []
  for (const card of incoming.cards) {
    const held = libraryCard(card.def.cardCode)
    if (!held || held.setId === target) continue
    found.push({ title: card.def.name.title, set: librarySet(held.setId)?.name ?? '' })
  }
  return found
})

/* A replace swaps the set's cards outright, so anything the set holds that the
 * file does not is going away. That is the part worth knowing before the yes,
 * and the part a one-line confirm could never say. */
const importDiff = computed(() => {
  const incoming = pendingImport.value
  /* Adding to a set takes nothing out of it, so there is no removal to count and
   * the split that says so does not apply. */
  if (incoming?.into) {
    const had = new Set(setCards(incoming.into.id).map((c) => c.def.cardCode))
    return {
      added: incoming.cards.filter((c) => !had.has(c.def.cardCode)).length,
      updated: incoming.cards.filter((c) => had.has(c.def.cardCode)).length,
      removed: 0,
    }
  }
  if (!incoming?.replacing) return null
  const existing = setCards(incoming.replacing.id)
  const had = new Set(existing.map((c) => c.def.cardCode))
  const bringing = new Set(incoming.cards.map((c) => c.def.cardCode))
  return {
    added: incoming.cards.filter((c) => !had.has(c.def.cardCode)).length,
    updated: incoming.cards.filter((c) => had.has(c.def.cardCode)).length,
    removed: existing.filter((c) => !bringing.has(c.def.cardCode)).length,
  }
})

/* What the file would do to the cards that are already there, card by card and
 * key by key. The counts above say how much lands; this says what it lands on,
 * which is the question when two libraries have drifted apart. */
const importReview = computed(() => {
  const incoming = pendingImport.value
  if (!incoming) return null
  const target = incoming.into ?? incoming.replacing
  if (!target) return null
  const existing = setCards(target.id).map((c) => c.def as any)
  const review = importChanges(existing, incoming.cards.map((c) => c.def as any))
  // Adding to a set takes nothing out of it, whatever the file leaves unsaid.
  return incoming.into ? { ...review, removed: [] } : review
})

// One card's keys open at a time: a def is long, and two are longer.
const openChange = ref<string | null>(null)
const toggleChange = (code: string) => (openChange.value = openChange.value === code ? null : code)

const changeLabel = (change: CardChange) =>
  change.changes.map((c) => c.key).join(', ')

/* Everything imported arrives as a set, replacing one of the same name rather
 * than merging into it. A file that names no set falls back to what the cards
 * claim, and failing that to the file's own name. */
async function reviewImport(
  file: File,
  parse: (text: string) => Promise<{
    name: string
    description?: string | null
    url?: string | null
    sourceCode: string | null
    cards: CustomCard[]
  }>,
  into: LibrarySet | null = null,
) {
  error.value = null
  status.value = null
  pendingImport.value = null
  importExpanded.value = false
  openChange.value = null
  portraitsCut.value = 0
  try {
    const { name, description, url, sourceCode, cards } = await parse(await file.text())
    if (!cards.length) {
      error.value = t(`${K}fileHasNoCards`)
      return
    }
    pendingImport.value = {
      fileName: file.name,
      name,
      description: description ?? null,
      url: url ?? null,
      sourceCode,
      cards,
      replacing:
        sets.value.find((s) => (sourceCode && s.sourceCode === sourceCode) || s.name === name) ??
        null,
      minisCut: portraitsCut.value,
      signatures: (() => {
        const summary = summarizeSignatures(cards)
        return summary.linked || summary.orphans ? summary : null
      })(),
      into,
    }
    // The panel takes focus so Escape backs out of it without a click first.
    await nextTick()
    importPanel.value?.focus()
  } catch (e) {
    console.error(e)
    error.value = t(`${K}importFailed`)
  }
}

async function commitImport() {
  const incoming = pendingImport.value
  if (!incoming) return
  cancelImport()
  busy.value = true
  try {
    /* Into a set you already have, one card at a time: the set import replaces a
     * set's whole contents, which is right for a set and wrong for adding to one.
     * Saving each card is the same path the editor saves by, so the destination's
     * name is stamped on and the card lands as yours. */
    const set = incoming.into
      ? await (async () => {
          for (const card of incoming.cards) await saveToLibrary(card, incoming.into!.id)
          return incoming.into!
        })()
      : await importSet({
          name: incoming.name,
          description: incoming.description,
          url: incoming.url,
          sourceCode: incoming.sourceCode,
          cards: incoming.cards,
        })
    /* Straight into the set, which is where you were going anyway: an import is
     * only ever the first half of "now let me look at what I just brought in".
     * The status line has its own slot in the editor's head, so it comes too. */
    openSet(set.id)
    status.value = t(`${K}imported`, {
      count: t(`${K}cardCount`, incoming.cards.length),
      name: set.name,
      minis: incoming.minisCut ? ` ${t(`${K}minisCut`, incoming.minisCut)}` : '',
    })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}importFailed`)
  } finally {
    busy.value = false
  }
}

const fileBaseName = (file: File) => file.name.replace(/\.[^.]+$/, '').replace(/\.arkhamcard$/, '')

/* One button for both formats. This app's own export carries cards under a
 * `def`; an arkham.build ("Arkham Card Maker") pool export carries raw
 * `type_code` rows instead, so the file says which it is. */
function isArkhamBuildExport(parsed: any): boolean {
  const rows = Array.isArray(parsed?.data?.cards)
    ? parsed.data.cards
    : Array.isArray(parsed?.cards)
      ? parsed.cards
      : Array.isArray(parsed)
        ? parsed
        : [parsed]
  return rows.some((row: any) => row && !row.def && typeof row.type_code === 'string')
}

/* Only the simple fields cross over from arkham.build -- name, type, class,
 * cost, stats, traits, art -- never ability text, which this app has no field
 * for at all. Those cards are coded deterministically from their arkham.build
 * id, so a deck built on arkham.build against them resolves against these rows
 * instead of going missing, and re-importing the same file updates them. */
/* The same read and the same review as a set import, with somewhere to put it. */
async function onImportInto(event: Event, set: LibrarySet) {
  const input = event.target as HTMLInputElement
  const file = input.files?.[0]
  input.value = ''
  if (!file) return
  await reviewImport(
    file,
    async (text) => {
      const { parseCardExport } = await import('@/arkham/customCardLibrary')
      return parseCardExport(text, fileBaseName(file))
    },
    set,
  )
}

async function onImport(event: Event) {
  const input = event.target as HTMLInputElement
  const file = input.files?.[0]
  input.value = ''
  if (!file) return
  await reviewImport(file, async (text) => {
    if (!isArkhamBuildExport(JSON.parse(text))) {
      const { parseCardExport } = await import('@/arkham/customCardLibrary')
      return parseCardExport(text, fileBaseName(file))
    }
    const {
      parseArkhamBuildCards,
      arkhamBuildCardToCustomCard,
      attachInvestigatorPortraits,
      linkSignatures,
    } = await import('@/arkham/arkhamBuildImport')
    const { packName, packCode, cards: rawCards } = parseArkhamBuildCards(text)
    const cards = rawCards.map((raw: any) => arkhamBuildCardToCustomCard(raw, packName))
    /* arkham.build points a signature at its investigator; the investigator has
     * to point back, or the deck never picks the cards up. Done before the
     * review so it can say who was linked to whom. */
    linkSignatures(cards)
    /* There is no mini in the export, so an investigator's is cut out of its own
     * card face. Said out loud in the review, because it is a guess at where the
     * art sits and the author may want to replace it. */
    portraitsCut.value = await attachInvestigatorPortraits(cards)
    return {
      name: packName ?? fileBaseName(file),
      sourceCode: packCode,
      cards,
    }
  })
}
</script>

<template>
  <div class="page-container">

  <!-- Your sets, full width. The editor is one card at a time, so it has no
       room to show a set; here a set can open up and show its cards. -->
  <CustomCardsPage
    v-if="!inSet"
    inline
    class="sets-page"
    :title="t(`${K}title`)"
    :lede="t(`${K}setsLede`)"
    :status="status"
    :error="error"
  >
    <template #actions>
      <form class="new-set" @submit.prevent="addSet">
        <input v-model="newSetName" type="text" :placeholder="t(`${K}newSetName`)" @keydown.stop />
        <button type="submit" class="go" :disabled="!newSetName.trim()">
          <font-awesome-icon icon="layer-group" />
          {{ t(`${K}addSet`) }}
        </button>
      </form>
      <label class="tool import" v-tooltip="t(`${K}importTooltip`)">
        <font-awesome-icon icon="upload" />
        <span>{{ t(`${K}import`) }}</span>
        <input type="file" accept="application/json,.json" @change="onImport" />
      </label>
    </template>

    <!-- What is in the file, before it lands. A browser confirm had room for a
         sentence, and an import is bigger than a sentence: it can replace a set
         you have and drop cards out of it. -->
    <section
      v-if="pendingImport"
      ref="importPanel"
      class="import-review"
      tabindex="-1"
      @keydown.esc="cancelImport"
    >
      <header>
        <h2>
          {{ pendingImport.into
            ? t(`${K}reviewAddTitle`, { name: pendingImport.into.name })
            : pendingImport.replacing
              ? t(`${K}reviewReplaceTitle`, { name: pendingImport.replacing.name })
              : t(`${K}reviewTitle`, { name: pendingImport.name }) }}
        </h2>
        <span class="from">{{ t(`${K}reviewFrom`, { file: pendingImport.fileName }) }}</span>
      </header>

      <p class="count">
        {{ t(`${K}cardCount`, pendingImport.cards.length) }}
        <span v-if="importDiff" class="split">
          <span class="added">{{ t(`${K}reviewAdded`, { n: importDiff.added }) }}</span>
          <!-- Counted by what differs, not by what is present in both: a file
               that brings a card back unchanged updates nothing. -->
          <span>{{ t(`${K}reviewUpdated`, { n: importReview ? importReview.updated.length : importDiff.updated }) }}</span>
          <span v-if="!pendingImport.into" :class="{ dropped: importDiff.removed > 0 }">
            {{ t(`${K}reviewDropped`, { n: importDiff.removed }) }}
          </span>
        </span>
      </p>

      <ul class="types">
        <li v-for="row in importBreakdown" :key="row.cardType">
          <span class="n">{{ row.count }}</span>{{ row.label }}
        </li>
      </ul>

      <p v-if="pendingImport.minisCut" class="note">{{ t(`${K}minisCut`, pendingImport.minisCut) }}</p>
      <p v-if="pendingImport.signatures?.orphans" class="note">
        {{ t(`${K}reviewSignatureOrphans`, pendingImport.signatures.orphans) }}
      </p>
      <p v-if="importDiff && importDiff.removed" class="warn">
        <font-awesome-icon icon="triangle-exclamation" />
        {{ t(`${K}reviewRemoves`, { name: pendingImport.replacing?.name }, importDiff.removed) }}
      </p>
      <p v-if="pendingImport.replacing && isSubscribed(pendingImport.replacing)" class="warn">
        <font-awesome-icon icon="triangle-exclamation" />
        {{ t(`${K}reviewSubscribed`, { name: pendingImport.replacing.name }) }}
      </p>

      <!-- Which cards change, and in what. The counts say how big the import
           is; this says what it is, which is the only form of it you can agree
           to when the two libraries have drifted. -->
      <div v-if="importReview && (importReview.updated.length || importReview.added.length || importReview.removed.length)" class="review-changes">
        <p v-if="importReview.identical" class="note">
          {{ importReview.identical }} unchanged
        </p>

        <ul class="changed">
          <li v-for="card in importReview.updated" :key="card.cardCode" class="changed-card">
            <button type="button" class="change-head" @click="toggleChange(card.cardCode)">
              <span class="mark changed">changed</span>
              <span class="who">{{ card.title }}</span>
              <span class="keys">{{ changeLabel(card) }}</span>
            </button>
            <div v-if="openChange === card.cardCode" class="change-body">
              <div v-for="field in card.changes" :key="field.key" class="field">
                <code class="field-key">{{ field.key }}</code>
                <div class="sides">
                  <pre class="was">{{ field.before }}</pre>
                  <pre class="now">{{ field.after }}</pre>
                </div>
              </div>
            </div>
          </li>
          <li v-for="card in importReview.added" :key="card.cardCode" class="changed-card">
            <span class="change-head plain">
              <span class="mark added">new</span>
              <span class="who">{{ card.title }}</span>
            </span>
          </li>
          <li v-for="card in importReview.removed" :key="card.cardCode" class="changed-card">
            <span class="change-head plain">
              <span class="mark removed">dropped</span>
              <span class="who">{{ card.title }}</span>
            </span>
          </li>
        </ul>
      </div>

      <!-- Clipped to one row by default: a pool export can run to hundreds of
           cards, and the whole wrapped grid is taller than the page. -->
      <div ref="importPreview" class="review-preview" :class="{ expanded: importExpanded }">
        <CardSetStrip :wrap="importExpanded" class="review-gallery" :cards="pendingImport.cards" />
      </div>
      <button
        v-if="importExpanded || importOverflows"
        type="button"
        class="link"
        @click="importExpanded = !importExpanded"
      >
        {{ importExpanded
          ? t(`${K}reviewShowLess`)
          : t(`${K}reviewShowAll`, { count: pendingImport.cards.length }) }}
      </button>

      <p v-if="importElsewhere.length" class="warn">
        <font-awesome-icon icon="triangle-exclamation" />
        {{ t(`${K}reviewElsewhere`, { count: importElsewhere.length, name: importElsewhere[0].title, set: importElsewhere[0].set }) }}
      </p>

      <div class="review-actions">
        <button type="button" class="confirm" :disabled="busy || importElsewhere.length > 0" @click="commitImport">
          {{ pendingImport.replacing ? t(`${K}reviewConfirmReplace`) : t(`${K}reviewConfirm`) }}
        </button>
        <button type="button" class="cancel" @click="cancelImport">
          {{ t(`${K}reviewCancel`) }}
        </button>
      </div>
    </section>

    <p v-if="!libraryLoaded" class="muted">{{ t(`${K}loading`) }}</p>

    <!-- Nothing to show, so say what the thing is instead. -->
    <section v-else-if="!sets.length" class="empty">
      <font-awesome-icon icon="layer-group" />
      <h2>{{ t(`${K}emptyTitle`) }}</h2>
      <p class="lede">{{ t(`${K}emptyLede`) }}</p>
    </section>

    <template v-else>
      <CardFilters
        v-model="filter"
        :cards="cards"
        :placeholder="t(`${K}filterPlaceholder`)"
      >
        <SegmentedToggle
          v-model="setOrder"
          :options="setOrderOptions"
          :label="t(`${K}orderLabel`)"
        />
      </CardFilters>

      <p v-if="!visibleSets.length" class="no-matches">
        <font-awesome-icon icon="search" />
        <span>
          {{ filter.query.trim()
            ? t(`${K}noMatches`, { query: filter.query.trim() })
            : t(`${K}noFilterMatches`) }}
        </span>
        <button type="button" class="link" @click="clearFilter(filter)">
          {{ t(`${K}clearFilter`) }}
        </button>
      </p>

      <ul v-else class="set-cards">
      <li v-for="set in visibleSets" :key="set.id" class="panel">
        <div class="set-row">
          <form
            v-if="renamingSetId === set.id"
            class="rename-form"
            @submit.prevent="commitRename(set)"
          >
            <input
              v-model="renameDraft"
              class="rename"
              type="text"
              :aria-label="t(`${K}setNameLabel`)"
              :placeholder="t(`${K}setNameLabel`)"
              @keydown.stop
              @keydown.esc="leaveDetails(set)"
            />
            <textarea
              v-model="describeDraft"
              class="describe"
              rows="2"
              :aria-label="t(`${K}setDescriptionLabel`)"
              :placeholder="t(`${K}setDescriptionPlaceholder`)"
              @keydown.stop
              @keydown.esc="leaveDetails(set)"
            ></textarea>
            <input
              v-model="linkDraft"
              class="set-url"
              type="url"
              inputmode="url"
              :aria-label="t(`${K}setUrlLabel`)"
              :placeholder="t(`${K}setUrlPlaceholder`)"
              @keydown.stop
              @keydown.esc="leaveDetails(set)"
            />
            <ExpansionDetails v-model="expansionDraft" />
            <div class="rename-actions">
              <button type="submit">{{ t(`${K}saveSetDetails`) }}</button>
              <button type="button" class="cancel" @click="leaveDetails(set)">
                {{ t(`${K}publishCancel`) }}
              </button>
            </div>
          </form>
          <div v-else class="set-identity">
            <button type="button" class="set-open" @click="openSet(set.id)">
              <!-- An expansion's own symbol, the way a printed box is filed.
                   Decorative: the name beside it already says which set. -->
              <span
                v-if="set.icon"
                class="expansion-icon"
                :style="{ '--set-icon-url': `url(${set.icon})` }"
                aria-hidden="true"
              ></span>
              <span class="name">{{ set.name }}</span>
            </button>
            <div class="set-facts">
              <MetaChip>{{ set.cardCount ? t(`${K}cardCount`, set.cardCount) : t(`${K}emptySetShort`) }}</MetaChip>
              <!-- The lock is the chip's own: a set you follow is one you can
                   read, play and update, and not edit. One chip rather than two,
                   because where it came from and why it is locked are the same
                   fact. -->
              <MetaChip
                v-if="isSubscribed(set)"
                tone="good"
                icon="lock"
                v-tooltip="t(`${K}lockedHelp`)"
              >
                {{ t(`${K}subscribedBadge`, { version: set.subscribedVersion }) }}
              </MetaChip>
              <!-- Listed and nothing pending is good news and needs no
                   sentence; only waiting and denied get the line below. -->
              <MetaChip
                v-if="isListed(set) && !isAwaitingReview(set) && !wasDenied(set)"
                tone="good"
                icon="store"
              >
                {{ t(`${K}listedChip`, { version: set.approvedVersion }) }}
              </MetaChip>
              <!-- Somebody else's set, taken through a link they sent. Links
                   back to it rather than carrying a take button: taking it
                   replaces every card in the copy, which wants the sentence
                   that page has room for. -->
              <router-link
                v-if="isFromShare(set)"
                class="from-share"
                :to="{ name: 'SharedCardSet', params: { token: set.sharedToken } }"
                v-tooltip="
                  set.sharedBy
                    ? t(`${K}sharedBy`, { author: set.sharedBy })
                    : t(`${K}fromShareHelp`)
                "
                @click.stop
              >
                <MetaChip
                  :tone="set.sharedBehind ? 'good' : 'plain'"
                  :icon="set.sharedBehind ? 'link' : 'lock'"
                >
                  {{ t(`${K}${set.sharedBehind ? 'fromShareBehind' : 'fromShareChip'}`) }}
                </MetaChip>
              </router-link>
              <!-- Shared by link, with how many people have used it. A link
                   nobody has opened says something different from one three
                   people are playing with, so the count is the chip. -->
              <MetaChip
                v-if="isShared(set)"
                icon="link"
                v-tooltip="t(`${K}sharedChipHelp`)"
              >
                {{
                  set.shareCount
                    ? t(`${K}sharedWithCount`, set.shareCount)
                    : t(`${K}sharedChip`)
                }}
              </MetaChip>
              <!-- Where the set lives in the world, shown as the host: it is
                   the only thing on this row that leads somewhere else, and
                   the whole address would crowd out the facts beside it. -->
              <a
                v-if="set.url"
                class="site-link"
                :href="set.url"
                target="_blank"
                rel="noopener noreferrer"
                @click.stop
              >
                <font-awesome-icon icon="external-link" />
                {{ setLinkLabel(set.url) }}
              </a>
            </div>
          </div>

          <div class="row-trailing">
            <button
              v-if="updateAvailable(set)"
              type="button"
              class="update"
              @click="update(set)"
            >
              <font-awesome-icon icon="refresh" />
              {{ t(`${K}updateTo`, { version: set.latestVersion }) }}
            </button>

            <!-- A locked copy of a *published* set: the one thing to do with it
                 beyond reading it and taking it again is to make it yours. A
                 copy taken from a share link has no such button -- that set is
                 unpublished, and it stays the author's. -->
            <button
              v-if="canUnlock(set)"
              type="button"
              class="unlock"
              v-tooltip="t(`${K}unlockHelp`)"
              @click="unlock(set)"
            >
              <font-awesome-icon icon="unlock" />
              {{ t(`${K}unlockSet`) }}
            </button>

            <div class="row-actions" role="group" :aria-label="t(`${K}setActions`)">
              <button
                v-if="!isLocked(set)"
                type="button"
                v-tooltip="publishTitle(set)"
                :aria-label="publishTitle(set)"
                @click="startPublish(set)"
              >
                <font-awesome-icon icon="store" />
              </button>
              <!-- Handing it to a few people rather than to everybody, which is
                   what an unfinished set wants. Beside publishing, because they
                   are the two ways a set leaves your account. -->
              <button
                v-if="!isLocked(set)"
                type="button"
                :class="{ on: isShared(set) }"
                v-tooltip="t(`${K}shareSet`, { name: set.name })"
                :aria-label="t(`${K}shareSet`, { name: set.name })"
                :aria-expanded="sharingSetId === set.id"
                @click="openShare(set)"
              >
                <font-awesome-icon icon="link" />
              </button>
              <button v-if="!isLocked(set)" type="button" v-tooltip="t(`${K}renameSet`)" :aria-label="t(`${K}renameSet`)" @click="startRename(set)">
                <font-awesome-icon icon="pen" />
              </button>
              <!-- Somebody else's unpublished work is not yours to hand on, so
                   a set taken from a share link has no export. Everything else
                   in your library does, your own shared sets included. -->
              <button
                v-if="canExport(set)"
                type="button"
                v-tooltip="t(`${K}exportSet`, { name: set.name })"
                :aria-label="t(`${K}exportSet`, { name: set.name })"
                @click="exportSet(set)"
              >
                <font-awesome-icon icon="download" />
              </button>
              <!-- Adds to this set rather than becoming one, which is what a card
                   somebody sent you needs: it has no set of its own to land as. -->
              <label
                v-if="!isLocked(set)"
                class="set-import"
                v-tooltip="t(`${K}importIntoSet`, { name: set.name })"
                :aria-label="t(`${K}importIntoSet`, { name: set.name })"
              >
                <font-awesome-icon icon="upload" />
                <input
                  type="file"
                  accept="application/json,.json"
                  @click.stop
                  @change="onImportInto($event, set)"
                />
              </label>
              <button
                type="button"
                class="delete"
                v-tooltip="t(`${K}deleteSet`)" :aria-label="t(`${K}deleteSet`)"
                @click="deletingSet = set"
              >
                <font-awesome-icon icon="trash" />
              </button>
            </div>
          </div>
        </div>

        <!-- What the set is, when its author has said. Under the row rather
             than in it, because it is a paragraph and the row is a line. -->
        <p v-if="set.description && renamingSetId !== set.id" class="set-description">
          {{ set.description }}
        </p>

        <!-- Where this set stands with the marketplace, for a set that has been
             submitted. A denial carries the reason, which is the whole point of
             having asked for one. -->
        <p
          v-if="isAwaitingReview(set) || wasDenied(set)"
          class="review"
          :class="{ denied: wasDenied(set) }"
        >
          <font-awesome-icon :icon="wasDenied(set) ? 'circle-xmark' : isAwaitingReview(set) ? 'hourglass-half' : 'store'" />
          <span>
            {{ reviewLine(set) }}
            <em v-if="wasDenied(set) && set.submissionReason">{{ set.submissionReason }}</em>
          </span>
        </p>

        <form
          v-if="publishingSetId === set.id"
          class="publish"
          @submit.prevent="commitPublish(set)"
        >
          <p class="publish-lede">
            {{ t(`${K}${userStore.isAdmin ? 'publishLede' : 'submitLede'}`) }}
          </p>
          <!-- What the shelf will say: the set's own blurb and link, shown
               rather than asked for. First, because it is the thing someone
               deciding whether to take the set actually reads; the note below it
               is only about this version. -->
          <div class="publish-details">
            <span>{{ t(`${K}publishDetailsLabel`) }}</span>
            <p class="blurb" :class="{ none: !set.description }">
              {{ set.description || t(`${K}noDescriptionYet`) }}
            </p>
            <a
              v-if="set.url"
              class="site-link"
              :href="set.url"
              target="_blank"
              rel="noopener noreferrer"
            >
              <font-awesome-icon icon="external-link" />
              {{ setLinkLabel(set.url) }}
            </a>
            <small>{{ t(`${K}publishDetailsHelp`) }}</small>
            <button type="button" class="edit-details" @click="editDetails(set)">
              <font-awesome-icon icon="pen" />
              {{ t(`${K}editSetDetails`) }}
            </button>
          </div>
          <input
            v-model="publishNote"
            type="text"
            :placeholder="t(`${K}publishNote`)"
            @keydown.stop
            @keydown.esc="publishingSetId = null"
          />
          <!-- Nothing to be emailed about a decision you are making yourself. -->
          <label v-if="!userStore.isAdmin" class="notify">
            <input v-model="publishNotify" type="checkbox" />
            <span>{{ t(`${K}notifyMe`) }}</span>
          </label>
          <div class="publish-actions">
            <button type="submit">
              {{ t(`${K}${userStore.isAdmin ? 'publishConfirm' : 'submitConfirm'}`) }}
            </button>
            <button type="button" class="cancel" @click="publishingSetId = null">
              {{ t(`${K}publishCancel`) }}
            </button>
          </div>
        </form>

        <!-- The link, and everyone who has used it. Under the row rather than in
             a dialog: it is a list that grows, and the author comes back to it. -->
        <section v-if="sharingSetId === set.id" class="share">
          <p class="share-lede">{{ t(`${K}shareLede`) }}</p>

          <template v-if="share">
            <div class="share-link">
              <input
                :value="shareLink"
                type="text"
                readonly
                :aria-label="t(`${K}shareLinkLabel`)"
                @focus="($event.target as HTMLInputElement).select()"
                @keydown.stop
              />
              <button type="button" class="copy" @click="copyLink">
                <font-awesome-icon icon="copy" />
                {{ t(`${K}${copied ? 'shareCopied' : 'shareCopy'}`) }}
              </button>
            </div>
            <small class="share-help">{{ t(`${K}shareLinkHelp`) }}</small>

            <!-- Who has it. A share is a url, so this is everyone who has opened
                 it -- there is no invitation list to compare it against. -->
            <h4 class="share-people-title">
              <font-awesome-icon icon="users" />
              {{ t(`${K}sharePeople`) }}
            </h4>
            <p v-if="!share.people.length" class="share-nobody">
              {{ t(`${K}shareNobodyYet`) }}
            </p>
            <ul v-else class="share-people">
              <li v-for="person in share.people" :key="person.id" :class="{ off: person.revoked }">
                <div class="who">
                  <span class="name">{{ person.name }}</span>
                  <span class="standing">{{ accessLine(person) }}</span>
                </div>
                <button
                  type="button"
                  class="quiet"
                  :disabled="shareBusy"
                  @click="setAccess(set, person, !person.revoked)"
                >
                  {{ t(`${K}${person.revoked ? 'shareAllowAgain' : 'shareRemovePerson'}`) }}
                </button>
              </li>
            </ul>

            <div class="share-actions">
              <!-- A new link, which makes the one you sent worthless. Asked
                   first, because somebody is holding the old one. -->
              <template v-if="confirmingRotate">
                <span class="confirm-line">{{ t(`${K}shareRotateWarning`) }}</span>
                <button type="button" :disabled="shareBusy" @click="rotateShare(set)">
                  {{ t(`${K}shareRotateConfirm`) }}
                </button>
                <button type="button" class="cancel" @click="confirmingRotate = false">
                  {{ t(`${K}publishCancel`) }}
                </button>
              </template>
              <template v-else-if="confirmingStop">
                <span class="confirm-line">{{ t(`${K}shareStopWarning`) }}</span>
                <button type="button" class="danger" :disabled="shareBusy" @click="stopShare(set)">
                  {{ t(`${K}shareStopConfirm`) }}
                </button>
                <button type="button" class="cancel" @click="confirmingStop = false">
                  {{ t(`${K}publishCancel`) }}
                </button>
              </template>
              <template v-else>
                <!-- What they will see when they open it, which is the one thing
                     the link itself cannot tell you. -->
                <router-link
                  v-if="share"
                  class="quiet preview"
                  :to="{ name: 'SharedCardSet', params: { token: share.token } }"
                >
                  <font-awesome-icon icon="eye" />
                  {{ t(`${K}sharePreview`) }}
                </router-link>
                <button type="button" class="quiet" @click="confirmingRotate = true">
                  <font-awesome-icon icon="refresh" />
                  {{ t(`${K}shareRotate`) }}
                </button>
                <button type="button" class="quiet danger" @click="confirmingStop = true">
                  <font-awesome-icon icon="link-slash" />
                  {{ t(`${K}shareStop`) }}
                </button>
                <button type="button" class="cancel" @click="openShare(set)">
                  {{ t(`${K}shareClose`) }}
                </button>
              </template>
            </div>
          </template>

          <p v-else-if="shareBusy" class="share-nobody">{{ t(`${K}loading`) }}</p>

          <!-- Not shared yet: one button, and what it will do. An empty set can
               be shared -- the link is made before the cards often enough -- and
               says so to whoever opens it until there is something in it. -->
          <div v-else class="share-actions">
            <button type="button" :disabled="shareBusy" @click="startShare(set)">
              <font-awesome-icon icon="link" />
              {{ t(`${K}shareStart`) }}
            </button>
            <button type="button" class="cancel" @click="openShare(set)">
              {{ t(`${K}publishCancel`) }}
            </button>
            <span v-if="!set.cardCount" class="confirm-line">{{ t(`${K}shareNeedsCards`) }}</span>
          </div>
        </section>

        <!-- As many cards as fit on one row, and no more: the grid's auto-fill
             decides how many that is, and the row below it is clipped. -->
        <SetPreview
          interactive
          :data-set-id="set.id"
          :cards="matchingCards(set.id)"
          :total="set.cardCount"
          :empty-label="filtering ? t(`${K}noCardMatches`) : t(`${K}emptySet`)"
          @pick="edit"
          @view-all="openSet(set.id)"
        />
      </li>
      </ul>
    </template>
  </CustomCardsPage>

  <div v-else class="card-builder">
    <aside class="library" :class="{ collapsed: libraryCollapsed }">
      <div class="library-content">
      <div class="library-head">
        <h2 class="panel-title" :title="activeSet?.name">{{ activeSet?.name }}</h2>
      </div>

      <button type="button" class="back-to-sets" @click="leaveSet">
        ← {{ t(`${K}backToSets`) }}
      </button>

      <!-- What this set declares, where the cards that use it are. Without this
           the tokens and keywords were only visible inside the set-details form
           on the other page, which is no way to check what a card's keyword
           refers to while writing it. Editable in place for the same reason. -->
      <!-- What this set declares, where the cards that use it are: a list, so a
           card's keyword can be checked against its rule while it is being
           written. Editing happens in the main panel, which has the room for
           it -- these only say what is there and open that editor. -->
      <!-- One way in to everything the set declares. The tokens and the
           keywords were two lists here with their own add buttons, which put a
           growing menu of kinds in a panel whose job is the cards; the editor
           they open has room for them and a tab each. -->
      <button
        v-if="activeSet"
        type="button"
        class="edit-expansion"
        :class="{ on: editingDeclaration }"
        :disabled="activeSetLocked"
        @click="startDeclaration(activeSet)"
      >
        <font-awesome-icon icon="box-archive" />
        <span class="edit-expansion-text">
          <strong>{{ t(`${K}editExpansion`) }}</strong>
          <small>{{ declarationSummary }}</small>
        </span>
      </button>

      <div class="group-head">
        <h3>
          {{ filtering && visibleCards.length !== activeCards.length
            ? t(`${K}cardCountFiltered`, { shown: visibleCards.length, total: activeCards.length })
            : t(`${K}cardCount`, activeCards.length) }}
        </h3>
        <!-- Selecting cards is only ever for exporting them, so a set with no
             export has no selection either. -->
        <div v-if="activeSetExportable" class="row-actions">
          <button
            v-if="!wholeSetOnly"
            type="button"
            class="select-toggle"
            :class="{ on: selecting }"
            :aria-pressed="selecting"
            :disabled="!visibleCards.length"
            v-tooltip="t(`${K}selectCards`)"
            :aria-label="t(`${K}selectCards`)"
            @click="toggleSelecting"
          >
            <font-awesome-icon icon="check-double" />
          </button>
          <button
            v-if="selecting"
            type="button"
            :disabled="!visibleCards.length"
            v-tooltip="t(`${K}selectAll`)"
            :aria-label="t(`${K}selectAll`)"
            @click="selectAll"
          >
            <font-awesome-icon icon="list" />
          </button>
          <!-- Offered whether or not anything is picked: with a selection it
               takes that, and without one it takes the set. -->
          <button
            type="button"
            :disabled="!activeCards.length"
            v-tooltip="selected.length
              ? t(`${K}exportSelected`, { count: selected.length })
              : t(`${K}exportSet`, { name: activeSet?.name })"
            :aria-label="selected.length
              ? t(`${K}exportSelected`, { count: selected.length })
              : t(`${K}exportSet`, { name: activeSet?.name })"
            @click="exportSelected"
          >
            <font-awesome-icon icon="download" />
          </button>
          <button
            v-if="selecting"
            type="button"
            :disabled="!selected.length"
            v-tooltip="t(`${K}clearSelection`)"
            :aria-label="t(`${K}clearSelection`)"
            @click="clearSelection"
          >
            <font-awesome-icon icon="times" />
          </button>
        </div>
      </div>

      <!-- A set that follows somebody else's stays what they published or
           shared, so there is nothing to add to it and nothing to throw away.
           For a copy of something published, the button that makes it yours sits
           where the new-card button was; for one taken from a share link there
           is no such button, and the box says so instead of leaving the missing
           controls to be puzzled over. -->
      <button
        v-if="activeSetUnlockable"
        type="button"
        class="unlock-panel"
        @click="unlock(activeSet!)"
      >
        <font-awesome-icon icon="unlock" />
        <span class="unlock-text">
          <strong>{{ t(`${K}unlockSet`) }}</strong>
          <small>{{ t(`${K}lockedPanelHelp`) }}</small>
        </span>
      </button>
      <p v-else-if="activeSetLocked" class="shared-note">
        <font-awesome-icon icon="lock" />
        <span>
          {{
            activeSet?.sharedBy
              ? t(`${K}sharedBy`, { author: activeSet.sharedBy })
              : t(`${K}sharedPanelHelp`)
          }}
        </span>
      </p>
      <button v-else type="button" class="new-card" @click="startNew">+ {{ t(`${K}newCard`) }}</button>

      <p v-if="!activeCards.length" class="muted">{{ t(`${K}emptySetPanel`) }}</p>
      <template v-else>
        <!-- A set runs to a few hundred cards once it is a box rather than a
             handful, and the panel is the only way to reach any of them. -->
        <CardFilters
          v-model="filter"
          compact
          class="panel-filters"
          :cards="activeCards"
          :placeholder="t(`${K}panelFilterPlaceholder`)"
        />

        <p v-if="!visibleCards.length" class="no-matches panel-no-matches">
          <font-awesome-icon icon="search" />
          <span>{{ t(`${K}noCardMatches`) }}</span>
          <button type="button" class="link" @click="clearFilter(filter)">
            {{ t(`${K}clearFilter`) }}
          </button>
        </p>

        <ul v-else class="library-list" :class="{ selecting: selecting && activeSetExportable }">
        <li
          v-for="card in visibleCards"
          :key="card.def.cardCode"
          :class="{ editing: editingCode === card.def.cardCode }"
        >
          <input
            v-if="selecting && activeSetExportable"
            type="checkbox"
            :checked="isSelected(card.def.cardCode)"
            @change="toggleSelected(card.def.cardCode)"
          />
          <!-- Titles are wider than the panel more often than not, so the
               whole name is a hover away rather than only the half that fits. -->
          <button
            type="button"
            class="library-card"
            :class="{ numbered }"
            :title="card.def.name.title"
            @click="edit(card)"
          >
            <!-- Kept even when this card has none, so the pictures beside it
                 line up with the ones above and below. -->
            <span v-if="numbered" class="number">{{ printedNumber(card) }}</span>
            <img :src="cardArt(card)" :data-image-id="card.def.cardCode" alt="" />
            <span class="text">
              <!-- Two cards in a set can share a title and differ only in
                   level, so the name says which one it is, the way the game
                   writes it. It sits beside the name rather than under it with
                   the type, where the panel's width would ellipsize away the
                   one digit that tells the two rows apart. A card with no level
                   says nothing rather than (0): an investigator is not a level
                   0 card. -->
              <span class="title">
                <span class="name">{{ card.def.name.title }}</span>
                <span v-if="card.def.level !== null && card.def.level !== undefined" class="level">
                  ({{ card.def.level }})
                </span>
              </span>
              <small>{{ cardKind(card.def.cardType) }}</small>
            </span>
          </button>
        </li>
        </ul>
      </template>
      </div>
    </aside>

    <!-- The seam between the panel and the editor: a full-height rule with the
         one control that opens and closes the panel sitting on it. -->
    <div v-if="!libraryCollapsed" class="library-seam">
      <button
        type="button"
        class="library-collapse"
        aria-expanded="true"
        :aria-label="t(`${K}hideLibrary`)"
        :title="t(`${K}hideLibrary`)"
        @click="libraryCollapsed = true"
      >
        <span class="collapse-glyph" aria-hidden="true" :data-tooltip="t(`${K}hideLibrary`)">«</span>
      </button>
    </div>

    <main class="builder">
      <header ref="headEl" class="builder-head" :class="{ stuck: headStuck }">
        <!-- With the panel shut the seam is gone, so the way back in sits with
             the title of whatever you are editing. -->
        <button
          v-if="libraryCollapsed"
          type="button"
          class="library-expand"
          aria-expanded="false"
          :aria-label="t(`${K}showLibrary`)"
          :title="t(`${K}showLibrary`)"
          @click="libraryCollapsed = false"
        >
          <font-awesome-icon class="toggle-arrow" icon="chevron-right" />
          <font-awesome-icon icon="layer-group" />
        </button>
        <h2>
          <template v-if="editingDeclaration">{{ t(`${K}investigatorExpansion`) }}</template>
          <!-- A locked set has no new card to start and nothing unsaved in it,
               so its resting state is "nothing picked" and its open state is
               just the card, read. -->
          <template v-else-if="activeSetLocked">
            {{ editingCode ? t(`${K}lockedReadingCard`) : t(`${K}lockedNothingPicked`) }}
          </template>
          <template v-else>
            {{
              editingCode
                ? editingTitle
                  ? t(`${K}editingNamedCard`, { name: editingTitle })
                  : t(`${K}editingCard`)
                : t(`${K}newCard`)
            }}
            <span v-if="dirty" class="unsaved" :title="t(`${K}unsavedTitle`)">
              <span class="unsaved-dot" aria-hidden="true"></span>
              {{ t(`${K}unsaved`) }}
            </span>
          </template>
          <small v-if="activeSet" class="in-set">{{ t(`${K}inSet`, { name: activeSet.name }) }}</small>
        </h2>
        <div class="builder-actions">
          <span v-if="status && !dirty" class="status">{{ status }}</span>
          <span v-if="error" class="error">{{ error }}</span>
          <!-- Nothing to save on a set that follows somebody else's. Said
               where the save button was, so the absence is an answer rather
               than a thing that failed to render. -->
          <span v-if="activeSetLocked" class="locked-note">
            <font-awesome-icon icon="lock" />
            {{ t(`${K}lockedEditorNote`) }}
          </span>
          <template v-else-if="editingDeclaration">
            <button type="button" class="save dirty" @click="saveDeclaration(activeSet!)">
              {{ t(`${K}saveSetDetails`) }}
            </button>
            <button type="button" class="secondary" @click="editingDeclaration = false">
              {{ t(`${K}publishCancel`) }}
            </button>
          </template>
          <button
            v-else
            type="button"
            class="save"
            :class="{ dirty }"
            :disabled="busy"
            @click="save"
          >
            {{ editingCode ? t(`${K}saveChanges`) : t(`${K}createCard`) }}
          </button>
          <!-- The def behind the form, for the card the form is showing. Here
               rather than on every row in the panel: reading a card's JSON is
               part of editing that card, not something done to a list. -->
          <button
            v-if="editingRaw"
            type="button"
            class="secondary"
            :class="{ on: !!rawCard }"
            :aria-pressed="!!rawCard"
            v-tooltip="t(`${K}rawCard`)"
            :aria-label="t(`${K}rawCard`)"
            @click="rawCard = rawCard ? null : editingRaw"
          >
            <font-awesome-icon icon="code" />
          </button>
          <button
            v-if="editingRaw && activeSetExportable && !wholeSetOnly"
            type="button"
            class="secondary"
            v-tooltip="t(`${K}exportCard`)"
            :aria-label="t(`${K}exportCard`)"
            @click="exportOne(editingRaw)"
          >
            <font-awesome-icon icon="download" />
          </button>
          <button
            v-if="editingRaw && !activeSetLocked"
            type="button"
            class="secondary delete"
            v-tooltip="t(`${K}deleteCard`)"
            :aria-label="t(`${K}deleteCard`)"
            @click="deletingCard = editingRaw"
          >
            <font-awesome-icon icon="trash" />
          </button>
          <!-- Only worth a slot up here while the panel's own "+ New card" is out
               of reach. -->
          <button
            v-if="!activeSetLocked && libraryCollapsed && editingCode"
            type="button"
            class="secondary"
            @click="startNew"
          >
            + {{ t(`${K}newCard`) }}
          </button>
        </div>
      </header>

      <template v-if="editingDeclaration">
        <ExpansionDetails v-model="expansionDraft" />
        <SetTextEditor v-if="activeSetId" :setId="activeSetId" :disabled="activeSetLocked" />
      </template>

      <!-- Nothing picked, in a set whose cards can only be read. An empty form
           here would be an invitation the set cannot accept. -->
      <p v-else-if="activeSetLocked && !editingCode" class="locked-empty">
        <font-awesome-icon icon="lock" />
        <span>{{ t(`${K}lockedPickHint`) }}</span>
      </p>

      <template v-else>
        <p v-if="editingCode && !rawCard" class="muted">{{ t(`${K}saveNote`) }}</p>

        <!-- The same card, as the server holds it. It takes the editing area
             rather than floating over it, since it is the editor for that card
             and not a dialog about it. The form stays mounted underneath, so
             anything typed into it is still there on the way back. -->
        <RawCardEditor
          v-if="rawCard"
          :def="rawCard.def as unknown as Record<string, unknown>"
          :title="rawCard.def.name.title"
          :readonly="activeSetLocked"
          :busy="rawBusy"
          @save="saveRaw"
          @close="rawCard = null"
        />

        <!-- A native disabled fieldset, which disables every control inside it:
             the card reads as it is rather than as a form that will not take. -->
        <fieldset v-show="!rawCard" class="card-form" :disabled="activeSetLocked">
          <CustomCardForm
            ref="form"
            :setKeywords="openSetDeclaration.keywords"
            :setTokens="openSetDeclaration.tokens"
          />
        </fieldset>
      </template>
    </main>
  </div>

  <!-- Document-level: anything carrying `data-image` gets a hover preview. -->
  <CardOverlay />

  <Prompt
    v-if="deletingSet"
    :prompt="deleteSetPrompt"
    :yes="() => dropSet(deletingSet!)"
    :no="() => (deletingSet = null)"
  />

  <Prompt
    v-if="deletingCard"
    :prompt="t(`${K}confirmDeleteCard`, { name: deletingCard.def.name.title })"
    :yes="() => remove(deletingCard!)"
    :no="() => (deletingCard = null)"
  />

  <Prompt
    v-if="unsavedAsk"
    :prompt="t(`${K}confirmDiscard`)"
    :yes="() => unsavedAsk?.(true)"
    :no="() => unsavedAsk?.(false)"
  />
  </div>
</template>

<style scoped lang="scss">
.page-container {
  height: 100%;
  overflow-x: hidden;
  overflow-y: auto;
  width: 100%;
}

/* The sets page: full width, because a set has cards to show and the editor
   next door only ever shows one. */
/* Nothing yet, said as an invitation rather than as a blank page. */
.empty {
  align-items: center;
  border: 1px dashed var(--box-border);
  border-radius: 8px;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin: 0;
  padding: 2.5rem 1.5rem;
  text-align: center;

  > svg {
    font-size: 1.8rem;
    opacity: 0.3;
  }

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }

  .lede {
    margin: 0;
    max-width: 52ch;
    opacity: 0.75;
  }
}

.no-matches {
  align-items: center;
  color: color-mix(in srgb, var(--title) 60%, transparent);
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  padding: 3rem 1rem;
  text-align: center;

  svg {
    font-size: 1.6rem;
    opacity: 0.4;
  }

  /* The way out of a filter that found nothing, not a second thing to try. */
  .link {
    background: none;
    border: none;
    color: var(--spooky-green);
    cursor: pointer;
    font-size: 0.8rem;
    padding: 0.2rem 0;
  }
}

/* The same sentence in the narrow panel, where three rems of padding above and
   below it is most of what you can see. */
.panel-no-matches {
  font-size: 0.8rem;
  gap: 0.4rem;
  padding: 1.2rem 0.5rem;

  svg {
    font-size: 1.1rem;
  }
}

/* The panel's own filters sit between the new-card button and the list, with
   the list's gap above them rather than their own below. */
.panel-filters {
  margin-top: 0.6rem;
}

.set-cards {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
  list-style: none;
  margin: 0;
  padding: 0;
}

.panel {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  overflow: hidden;
  transition: border-color 150ms ease;

  &:hover {
    border-color: color-mix(in srgb, var(--box-border) 40%, var(--background-mid));
  }
}

.set-row {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem 0.75rem;
  justify-content: space-between;
  padding: 0.7rem 0.9rem 0.5rem;

  .rename {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-size: 1rem;
    min-width: 0;
    padding: 0.2rem 0.4rem;
    width: 100%;
  }

  /* Takes the whole row while it is open: a name and a paragraph do not sit
     beside the row's buttons, and the buttons act on the set rather than on
     what is being typed. */
  .rename-form {
    display: flex;
    flex: 1 1 100%;
    flex-direction: column;
    gap: 0.4rem;
    min-width: 0;
  }

  .describe,
  .set-url {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-family: inherit;
    font-size: 0.85rem;
    min-width: 0;
    padding: 0.3rem 0.45rem;
    resize: vertical;
    width: 100%;
  }

  .rename-actions {
    display: flex;
    gap: 0.4rem;

    button {
      font-size: 0.8rem;
      padding: 0.25rem 0.7rem;
    }

    .cancel {
      background: none;
    }
  }
}

/* What the set is. Sits under the row like the review line does, and keeps the
   author's own line breaks: a blurb is often a sentence and a list. */
/* The name and what it is, left; the things you can do to it, right. Split so
   the row keeps its shape when the name is long -- it used to push the action
   rail onto a line of its own halfway through a word. */
.set-identity {
  display: flex;
  flex: 1 1 18rem;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 0.3rem 0.6rem;
  min-width: 0;
}

.set-facts {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
}

/* Sits among the chips but is not one: it is the only thing on the row that
   leads off the page, so it reads as a link rather than as a fact. */
.site-link {
  align-items: center;
  color: var(--spooky-green);
  display: inline-flex;
  font-size: 0.75rem;
  gap: 0.3rem;
  text-decoration: none;

  &:hover {
    text-decoration: underline;
  }
}

.row-trailing {
  align-items: center;
  display: flex;
  flex: none;
  gap: 0.5rem;
}

.set-description {
  color: color-mix(in srgb, var(--title) 78%, transparent);
  font-size: 0.85rem;
  line-height: 1.45;
  margin: 0;
  max-width: 64ch;
  padding: 0 0.9rem 0.6rem;
  white-space: pre-wrap;
}

.update {
  align-items: center;
  background: none;
  border: 1px solid color-mix(in srgb, var(--spooky-green) 60%, transparent);
  border-radius: 5px;
  color: var(--spooky-green);
  cursor: pointer;
  display: inline-flex;
  flex: none;
  font-size: 0.78rem;
  gap: 0.35rem;
  min-height: 32px;
  padding: 0 0.6rem;
  white-space: nowrap;

  &:hover {
    background: color-mix(in srgb, var(--spooky-green) 16%, transparent);
  }
}

/* Where the set stands with the marketplace. Reads as a note on the row rather
 * than an alert: for a listed set it is good news, and it is on screen always. */
.review {
  align-items: flex-start;
  border-top: 1px solid var(--box-border);
  color: color-mix(in srgb, var(--title) 75%, transparent);
  display: flex;
  font-size: 0.78rem;
  gap: 0.45rem;
  margin: 0;
  padding: 0.5rem 0.9rem;

  svg {
    margin-top: 0.15rem;
    opacity: 0.8;
  }

  em {
    color: color-mix(in srgb, var(--title) 90%, transparent);
    display: block;
    font-style: italic;
  }

  &.denied {
    color: color-mix(in srgb, var(--survivor) 70%, white);
  }
}

.publish {
  border-top: 1px solid var(--box-border);
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
  padding: 0.6rem 0.75rem;

  /* The form says what submitting does, because it no longer does what the word
     "publish" promised: it asks somebody to look at the set. */
  .publish-lede {
    color: color-mix(in srgb, var(--title) 70%, transparent);
    flex: 1 1 100%;
    font-size: 0.75rem;
    margin: 0;
  }

  .notify {
    align-items: center;
    color: color-mix(in srgb, var(--title) 85%, transparent);
    cursor: pointer;
    display: flex;
    flex: 1 1 100%;
    font-size: 0.78rem;
    gap: 0.4rem;

    input[type="checkbox"] {
      accent-color: var(--spooky-green);
      flex: none;
      margin: 0;
      width: auto;
    }
  }

  .publish-actions {
    display: flex;
    gap: 0.4rem;
  }

  /* What the shelf will say, quoted back rather than typed again: a panel the
     set's own words sit in, with the way to change them under it. */
  .publish-details {
    align-items: flex-start;
    background: rgba(0, 0, 0, 0.18);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    display: flex;
    flex: 1 1 100%;
    flex-direction: column;
    gap: 0.3rem;
    padding: 0.45rem 0.55rem;

    > span {
      color: color-mix(in srgb, var(--title) 85%, transparent);
      font-size: 0.78rem;
    }

    .blurb {
      color: color-mix(in srgb, var(--title) 85%, transparent);
      font-size: 0.85rem;
      line-height: 1.45;
      margin: 0;
      max-width: 64ch;
      white-space: pre-wrap;

      /* Nothing written yet reads as the gap it is, not as the blurb. */
      &.none {
        color: color-mix(in srgb, var(--title) 55%, transparent);
        font-style: italic;
      }
    }

    small {
      color: color-mix(in srgb, var(--title) 60%, transparent);
      font-size: 0.72rem;
    }

    /* Leaves the form for the one that owns these words, so it is not one of
       the two buttons that act on the submission. */
    .edit-details {
      background: none;
      border: 1px solid var(--box-border);
      color: color-mix(in srgb, var(--title) 85%, transparent);
      display: inline-flex;
      gap: 0.35rem;
      padding: 0.2rem 0.5rem;
    }
  }

  /* The note shares its row with the buttons, so the basis here is a width. */
  > input {
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    flex: 1 1 18rem;
    font-size: 0.85rem;
    min-width: 0;
    padding: 0.3rem 0.5rem;
  }

  button {
    font-size: 0.8rem;
    padding: 0.3rem 0.7rem;
  }

  .cancel {
    background: none;
  }
}

/* A fieldset only here to carry `disabled`, so it must not look like one. */
.card-form {
  border: 0;
  margin: 0;
  min-width: 0;
  padding: 0;
}

/* The way out of a locked set, where the thing it replaces was: beside the row's
   other actions on the sets page, and where "+ New card" sits in the panel. */
.unlock {
  align-items: center;
  background: none;
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  cursor: pointer;
  display: inline-flex;
  font-size: 0.8rem;
  gap: 0.4rem;
  padding: 0.3rem 0.7rem;

  &:hover {
    border-color: var(--background-mid);
  }
}

.unlock-panel {
  align-items: center;
  background: none;
  border: 1px solid var(--box-border);
  border-radius: 6px;
  color: var(--title);
  cursor: pointer;
  display: flex;
  gap: 0.5rem;
  margin-bottom: 0.6rem;
  padding: 0.5rem 0.65rem;
  text-align: left;
  width: 100%;

  &:hover {
    border-color: var(--background-mid);
  }

  .unlock-text {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
    min-width: 0;

    small {
      color: color-mix(in srgb, var(--title) 60%, transparent);
      font-size: 0.72rem;
      line-height: 1.3;
    }
  }
}

/* The editor with nothing picked, in a set that can only be read. */
.locked-empty {
  align-items: center;
  color: color-mix(in srgb, var(--title) 60%, transparent);
  display: flex;
  gap: 0.5rem;
  justify-content: center;
  padding: 3rem 1rem;

  svg {
    opacity: 0.6;
  }
}

/* Why a shared set has no "make it mine" button, where that button would be. */
.shared-note {
  align-items: baseline;
  border: 1px solid var(--box-border);
  border-radius: 6px;
  color: color-mix(in srgb, var(--title) 65%, transparent);
  display: flex;
  font-size: 0.78rem;
  gap: 0.45rem;
  line-height: 1.35;
  margin: 0 0 0.6rem;
  padding: 0.5rem 0.65rem;

  svg {
    flex: none;
    font-size: 0.85em;
    opacity: 0.7;
  }
}

/* Where the save button would be, saying why there isn't one. */
.locked-note {
  align-items: center;
  color: color-mix(in srgb, var(--title) 65%, transparent);
  display: inline-flex;
  font-size: 0.8rem;
  gap: 0.4rem;

  svg {
    font-size: 0.85em;
    opacity: 0.7;
  }
}

/* A chip that is a link. The chip carries all the appearance; this only has to
   stop the anchor adding an underline and a color of its own. */
.from-share {
  color: inherit;
  display: inline-flex;
  text-decoration: none;
}

/* The share panel. Same seam and same paddings as the publish form above it:
   they are the two things you do with a whole set, and they sit in the same
   place under the same row. */
.share {
  border-top: 1px solid var(--box-border);
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
  padding: 0.6rem 0.75rem;

  .share-lede,
  .share-help,
  .share-nobody {
    color: color-mix(in srgb, var(--title) 70%, transparent);
    font-size: 0.75rem;
    margin: 0;
  }

  .share-nobody {
    font-style: italic;
  }

  /* The link, wide, selectable and read-only: it is something to copy, never to
     edit, and a field you can put a cursor in is the one people reach for when
     the button does not work. */
  .share-link {
    display: flex;
    gap: 0.4rem;

    input {
      background: rgba(0, 0, 0, 0.25);
      border: 1px solid var(--box-border);
      border-radius: 4px;
      color: var(--title);
      flex: 1 1 18rem;
      font-family: monospace;
      font-size: 0.8rem;
      min-width: 0;
      padding: 0.3rem 0.5rem;
    }
  }

  .share-people-title {
    align-items: center;
    color: color-mix(in srgb, var(--title) 85%, transparent);
    display: flex;
    font-size: 0.78rem;
    font-weight: normal;
    gap: 0.35rem;
    margin: 0.3rem 0 0;

    svg {
      opacity: 0.7;
    }
  }

  .share-people {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
    list-style: none;
    margin: 0;
    padding: 0;

    li {
      align-items: center;
      background: rgba(0, 0, 0, 0.18);
      border: 1px solid var(--box-border);
      border-radius: 4px;
      display: flex;
      gap: 0.5rem;
      justify-content: space-between;
      padding: 0.3rem 0.5rem;

      /* Somebody who has been removed stays on the list -- that they had it is
         the fact worth keeping -- but reads as past tense. */
      &.off {
        opacity: 0.5;
      }
    }

    .who {
      display: flex;
      flex-wrap: wrap;
      gap: 0.1rem 0.5rem;
      min-width: 0;
    }

    .name {
      font-size: 0.85rem;
    }

    .standing {
      color: color-mix(in srgb, var(--title) 60%, transparent);
      font-size: 0.75rem;
    }
  }

  .share-actions {
    align-items: center;
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem;
  }

  /* What a button is about to do, where the buttons are: a confirmation the eye
     reaches before the hand does. */
  .confirm-line {
    color: color-mix(in srgb, var(--title) 70%, transparent);
    flex: 1 1 100%;
    font-size: 0.75rem;
  }

  button {
    align-items: center;
    display: inline-flex;
    font-size: 0.8rem;
    gap: 0.35rem;
    padding: 0.3rem 0.7rem;
  }

  .quiet,
  .cancel {
    background: none;
    border: 1px solid var(--box-border);
    color: color-mix(in srgb, var(--title) 85%, transparent);
  }

  /* A link wearing a button's clothes: same row, same job. */
  .preview {
    align-items: center;
    border-radius: 4px;
    display: inline-flex;
    font-size: 0.8rem;
    gap: 0.35rem;
    padding: 0.3rem 0.7rem;
    text-decoration: none;
  }

  .cancel {
    border-color: transparent;
  }

  /* Red only where something stops working for other people. */
  .danger {
    color: var(--survivor);
  }
}

/* `baseline`, not `center`: the chips beside this button line up on the set
   name's baseline, and a centred flex container has no baseline of its own to
   give them -- they dropped to the bottom of the button. The symbol opts out
   with `align-self` so the name is what the row aligns to. */
.set-open {
  align-items: baseline;
  background: none;
  border: none;
  border-radius: 4px;
  color: inherit;
  cursor: pointer;
  display: flex;
  gap: 0.4rem;
  max-width: 100%;
  min-width: 0;
  padding: 0;
  text-align: left;

  /* A silhouette masked out of a solid fill rather than drawn as an image, so
     the symbol takes the name's colour exactly. Sized to the name rather than
     fixed, so it stays in proportion whatever the type scale does. */
  .expansion-icon {
    align-self: center;
    background: currentColor;
    flex: none;
    height: 1.5em;
    -webkit-mask: var(--set-icon-url) center / contain no-repeat;
    mask: var(--set-icon-url) center / contain no-repeat;
    width: 1.5em;
  }

  .name {
    display: block;
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    line-height: 1.15;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  /* The symbol lights with the name: the whole button is the hover target, so
     leaving it behind would read as the symbol not being part of it. */
  &:hover .expansion-icon {
    color: white;
  }

  &:hover .name {
    color: white;
    text-decoration: underline;
  }

  &:focus-visible {
    outline: 2px solid var(--spooky-green);
    outline-offset: 2px;
  }
}

.card-builder {
  display: flex;
  gap: 2.5rem;
  align-items: flex-start;
  padding: 1.5rem;
  color: var(--title);
  /* So the seam between the panel and the editor reaches the bottom of the
     screen even when the card being edited is short. Percentage rather than a
     vh calc: everything here is border-box, so this already accounts for the
     padding the seam's negative margins reach back through. */
  min-height: 100%;

  @media (max-width: 900px) {
    flex-direction: column;
    min-height: 0;
  }
}

/* Collapsing slides the panel shut rather than snapping it: the content keeps
   its width and is clipped as the panel narrows, so nothing reflows on the way
   out. `flex-basis`, the padding and the border all move together. */
.library {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  flex: 0 0 380px;
  overflow: hidden;
  padding: 1rem;
  position: sticky;
  top: 0;
  transition:
    flex-basis 0.22s cubic-bezier(0.4, 0, 0.2, 1),
    padding 0.22s cubic-bezier(0.4, 0, 0.2, 1),
    border-width 0.22s cubic-bezier(0.4, 0, 0.2, 1),
    opacity 0.18s ease;

  &.collapsed {
    border-width: 0;
    flex-basis: 0;
    opacity: 0;
    padding-left: 0;
    padding-right: 0;
    pointer-events: none;
  }

  @media (max-width: 900px) {
    flex: 1 1 auto;
    position: static;
    width: 100%;

    /* Nothing to collapse into when the panel is stacked above the editor. */
    &.collapsed {
      border-width: 1px;
      flex-basis: auto;
      opacity: 1;
      padding: 1rem;
      pointer-events: auto;
    }
  }
}

.library-content {
  max-height: calc(100vh - var(--nav-height) - 5rem);
  overflow: auto;
  /* The panel's 380px less its padding. Fixed, so the text does not re-wrap
     while the panel is sliding shut. */
  width: 348px;

  @media (max-width: 900px) {
    max-height: none;
    width: auto;
  }
}

.library-seam {
  align-self: stretch;
  background: var(--box-border);
  flex: 0 0 1px;
  /* The negative block margins reach through the page's own padding, so the
     rule runs from the menu bar to the bottom of the screen. */
  margin: -1.5rem -1.25rem;
  position: relative;
  transition: background-color 0.15s ease;

  @media (max-width: 900px) {
    display: none;
  }
}

/* Both controls are the cards page's, unchanged: a hover-revealed glyph on the
   seam to close, and a labelled toggle in the header to open. */
.library-collapse {
  /* The glyph is pinned by `sticky`, not centred in the strip. */
  align-items: flex-start;
  background: transparent;
  border: 0;
  color: #aaa;
  cursor: pointer;
  display: inline-flex;
  height: 100%;
  justify-content: center;
  left: 50%;
  opacity: 0;
  padding: 0;
  position: absolute;
  top: 0;
  transform: translateX(-50%);
  transition: opacity 0.15s, color 0.15s;
  width: 18px;
  z-index: 4;

  &:hover,
  &:focus-visible {
    color: #fff;
    opacity: 1;
  }

  /* Sticky, not centred on the rule: the rule runs the whole page, so its middle
     scrolls away. Half the scrollport -- the viewport less the nav bar above it
     -- keeps the handle mid-screen and stationary. */
  .collapse-glyph {
    align-items: center;
    background: color-mix(in srgb, var(--background) 74%, white 26%);
    border: 1px solid rgba(255, 255, 255, 0.22);
    border-radius: 999px;
    box-shadow: 0 4px 14px rgba(0, 0, 0, 0.3);
    color: #fff;
    display: inline-flex;
    flex: none;
    font-size: 18px;
    font-weight: 800;
    height: 24px;
    justify-content: center;
    letter-spacing: 0;
    line-height: 1;
    position: sticky;
    text-shadow: 0 1px 2px rgba(0, 0, 0, 0.55);
    top: calc((100vh - var(--nav-height)) / 2 - 12px);
    width: 24px;
    z-index: 1;
  }

  .collapse-glyph:hover,
  &:focus-visible .collapse-glyph {
    background: color-mix(in srgb, var(--background) 72%, white 28%);
  }

  .collapse-glyph::after {
    background: rgba(12, 16, 18, 0.96);
    border: 1px solid rgba(255, 255, 255, 0.14);
    border-radius: 6px;
    box-shadow: 0 8px 20px rgba(0, 0, 0, 0.35);
    color: #eee;
    content: attr(data-tooltip);
    font-size: 0.72rem;
    font-weight: 600;
    left: 28px;
    letter-spacing: 0;
    line-height: 1;
    opacity: 0;
    padding: 5px 8px;
    pointer-events: none;
    position: absolute;
    text-shadow: none;
    top: 50%;
    transform: translateY(-50%) translateX(-4px);
    transition: opacity 0.12s, transform 0.12s;
    white-space: nowrap;
    z-index: 2;
  }

  .collapse-glyph:hover::after,
  &:focus-visible .collapse-glyph::after {
    opacity: 1;
    transform: translateY(-50%);
  }
}

/* The rule brightens while the handle is live, the way the cards page's sidebar
   border does. */
.library-seam:has(.library-collapse:hover),
.library-seam:has(.library-collapse:focus-visible) {
  background: rgba(255, 255, 255, 0.35);
}

.library-expand {
  align-items: center;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid rgba(255, 255, 255, 0.15);
  border-radius: 6px;
  color: #aaa;
  cursor: pointer;
  display: flex;
  flex-shrink: 0;
  gap: 3px;
  height: 32px;
  justify-content: center;
  padding: 0 8px;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
    color: #eee;
  }

  .toggle-arrow {
    display: inline-block;
    font-size: 0.65em;
    opacity: 0.7;
  }
}

@media (prefers-reduced-motion: reduce) {
  .library,
  .library-collapse {
    transition: none;
  }
}

.library-head {
  align-items: center;
  display: flex;
  margin-bottom: 0.6rem;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }
}

/* The title says where you are -- "Sets", or the name of the set you opened.
 * It gets the whole line, because a set name is the one thing here that can be
 * long, and the way back is its own control below rather than a second thing
 * competing for this row. */
.panel-title {
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

/* One way in to everything the set declares. Sits with the panel's own buttons
   rather than above the card list, because it is about the set and not about
   which card is open. */
.edit-expansion {
  align-items: center;
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  display: flex;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
  padding: 0.35rem 0.5rem;
  text-align: left;
  width: 100%;

  &:hover,
  &.on {
    background: rgba(255, 255, 255, 0.09);
  }

  svg {
    flex: none;
    opacity: 0.7;
  }
}

.edit-expansion-text {
  display: flex;
  flex-direction: column;
  min-width: 0;

  strong {
    font-size: 0.78rem;
    font-weight: 600;
  }

  small {
    font-size: 0.68rem;
    opacity: 0.6;
  }
}

.back-to-sets {
  background: rgba(255, 255, 255, 0.04);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.75rem;
  margin-bottom: 0.5rem;
  opacity: 0.75;
  padding: 0.25rem 0.5rem;
  text-align: left;
  width: 100%;

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    opacity: 1;
  }
}

.new-card {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
  margin-bottom: 0.5rem;
  padding: 0.25rem 0.55rem;
  white-space: nowrap;
  width: 100%;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
  }
}

/* The page's two top-level actions, the same height as each other and as the
   field beside them. They used to be two different heights and two different
   greys, which is most of why the header read as unfinished. */
.tool {
  align-items: center;
  background: none;
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  cursor: pointer;
  display: inline-flex;
  font-size: 0.85rem;
  gap: 0.4rem;
  justify-content: center;
  min-height: 38px;
  padding: 0 0.85rem;
  text-align: center;
  transition: border-color 120ms ease;

  &:hover:not(:disabled) {
    border-color: var(--background-mid);
  }

  &:focus-within {
    border-color: var(--spooky-green);
  }

  &:disabled {
    cursor: default;
    opacity: 0.4;
  }
}

.import input {
  height: 0;
  opacity: 0;
  position: absolute;
  width: 0;
}

.new-set {
  display: flex;
  gap: 0.4rem;

  @media (max-width: 700px) {
    flex: 1 1 auto;
  }

  input {
    background: var(--background-dark);
    border: 1px solid var(--box-border);
    border-radius: 5px;
    color: var(--title);
    font-size: 0.9rem;
    min-width: 0;
    padding: 0 0.6rem;
    width: 13rem;

    &:focus {
      border-color: var(--spooky-green);
      outline: none;
    }

    @media (max-width: 700px) {
      flex: 1 1 auto;
      width: auto;
    }
  }

  .go {
    align-items: center;
    background: var(--button-1);
    border: 1px solid transparent;
    border-radius: 5px;
    color: white;
    cursor: pointer;
    display: inline-flex;
    font-size: 0.85rem;
    gap: 0.4rem;
    min-height: 38px;
    padding: 0 0.85rem;
    white-space: nowrap;

    &:hover:not(:disabled) {
      background: var(--button-1-highlight);
    }

    &:disabled {
      cursor: default;
      opacity: 0.4;
    }
  }
}

/* The margin, rather than a leading `&#32;`: whether that entity survives
   depends on whether the branch before it ends in an element or in text, which
   is not a thing a space should depend on. */
.in-set {
  font-family: sans-serif;
  font-size: 0.6em;
  margin-left: 0.5em;
  opacity: 0.6;
}

.group-head {
  align-items: center;
  border-bottom: 1px solid var(--box-border);
  display: flex;
  gap: 0.4rem;
  margin: 1rem 0 0.4rem;
  padding-bottom: 0.25rem;

  &:first-child {
    margin-top: 0;
  }
}

.group-head .row-actions {
  margin-left: auto;
}

.group-count {
  font-size: 0.7rem;
  opacity: 0.5;
}

.group-head h3 {
  font-size: 0.75rem;
  min-width: 0;
  letter-spacing: 0.06em;
  margin: 0;
  opacity: 0.7;
  overflow: hidden;
  text-overflow: ellipsis;
  text-transform: uppercase;
  white-space: nowrap;
}

.library-list.selecting li {
  grid-template-columns: auto minmax(0, 1fr);
}

.library-list {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  list-style: none;
  margin: 0;
  /* Right padding keeps the scrollbar clear of the row buttons. */
  padding: 0 0.25rem 0 0;

  /* The card list is the one part of the library that grows without bound: a
   * full imported set ran to a few thousand pixels and took the page with it.
   * Cap it and give it its own scroller, so the head, tools and set list stay
   * put while only the cards move. `contain` stops a scroll that reaches the
   * end of the list from chaining on to the page behind it. */
  max-height: 60vh;
  overflow-y: auto;
  overscroll-behavior: contain;

  /* The card, and a checkbox in front of it while picking. What you do to a
   * card is done in the editor, where the card you are doing it to is open, so
   * the row has nothing in it but the card. */
  li {
    align-items: center;
    border: 1px solid transparent;
    border-radius: 6px;
    display: grid;
    gap: 0.5rem;
    grid-template-columns: minmax(0, 1fr);
    padding: 0.3rem 0.35rem;

    &:hover {
      background: rgba(255, 255, 255, 0.04);
    }

    &.editing {
      background: rgba(255, 255, 255, 0.06);
      border-color: var(--spooky-green);
    }
  }

  input[type='checkbox'] {
    margin: 0;
  }
}

.library-card {
  align-items: center;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: grid;
  gap: 0.55rem;
  grid-template-columns: 52px minmax(0, 1fr);
  min-width: 0;
  padding: 0;
  text-align: left;

  &.numbered {
    gap: 0.4rem 0.45rem;
    grid-template-columns: 2.1rem 52px minmax(0, 1fr);
  }

  /* One width for every row, so the numbers read as a column of the panel
     rather than as a label stuck on the front of each card. */
  .number {
    color: #9ca3af;
    flex: none;
    font-size: 0.72rem;
    font-variant-numeric: tabular-nums;
    overflow: hidden;
    text-align: left;
    text-overflow: ellipsis;
  }

  /* A box wide enough for the wider shape, filled by whichever shape the card
     is. An investigator is landscape, and cropped to a portrait thumbnail it
     was a slice out of the middle of its own art -- the one card in the panel
     whose picture said nothing about which card it was. */
  img {
    border-radius: 3px;
    height: 46px;
    object-fit: contain;
    width: 52px;
  }

  /* Name over type rather than beside it: the name gets the whole width to
   * ellipsize into. */
  .text {
    display: flex;
    flex-direction: column;
    min-width: 0;
  }

  /* The name gives way first: the level is one character wide and is the whole
     reason the row is distinguishable, so it keeps its width. */
  .title {
    display: flex;
    gap: 0.25rem;
    min-width: 0;
  }

  .name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .level {
    flex: none;
    opacity: 0.55;
  }

  small {
    font-size: 0.7rem;
    opacity: 0.55;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
}

/* One rail rather than five loose glyphs: bordered so it reads as a group of
   things you can do to this set, and sized so each one is a real target on a
   phone. The old 24px squares were below anything you could reliably hit. */
.row-actions {
  background: color-mix(in srgb, black 18%, transparent);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  display: flex;
  gap: 0.1rem;
  padding: 0.15rem;
}

/* Sized by pointer rather than by width: a tablet is wide and still has no
   cursor, and a 30px glyph is not something you can reliably hit with a thumb.
   The glyphs keep their size; only the targets around them grow. */
@media (pointer: coarse) {
  .row-actions button,
  .row-actions .set-import {
    height: 42px;
    width: 44px;
  }
}

/* A file input wearing the same clothes as its neighbours: a label rather than a
   button, because that is what opens a file picker, so it opts into the styling
   the buttons beside it get by tag. */
.row-actions .set-import {
  align-items: center;
  border-radius: 4px;
  cursor: pointer;
  display: grid;
  font-size: 0.8rem;
  height: 30px;
  line-height: 1;
  opacity: 0.55;
  place-items: center;
  width: 32px;

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    opacity: 1;
  }

  &:focus-within {
    background: rgba(255, 255, 255, 0.1);
    opacity: 1;
    outline: 2px solid var(--spooky-green);
    outline-offset: -2px;
  }

  input {
    height: 0;
    opacity: 0;
    position: absolute;
    width: 0;
  }
}

.row-actions button {
  background: none;
  border: none;
  border-radius: 4px;
  color: inherit;
  cursor: pointer;
  display: grid;
  font-size: 0.8rem;
  height: 30px;
  line-height: 1;
  opacity: 0.55;
  padding: 0;
  place-items: center;
  width: 32px;

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    opacity: 1;
  }

  &:focus-visible {
    background: rgba(255, 255, 255, 0.1);
    opacity: 1;
    outline: 2px solid var(--spooky-green);
    outline-offset: -2px;
  }

  &.delete:hover {
    color: var(--delete);
  }

  &.on {
    color: var(--spooky-green);
    opacity: 1;
  }
}

.builder {
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  flex: 1 1 auto;
  min-width: 0;
  padding: 1rem;
}

/* The import review. Sits between the page head and the sets it is about to
   change, and reads top-down: what lands, what it costs, what it looks like. */
/* The card-by-card half of a review: a list that stays a list until one row is
   opened, since the whole of a set's defs is longer than the page by far. */
.review-changes {
  margin-top: 0.6rem;
  max-height: 22rem;
  overflow-y: auto;

  .changed {
    display: flex;
    flex-direction: column;
    gap: 0.15rem;
    list-style: none;
    margin: 0.3rem 0 0;
    padding: 0;
  }
}

.change-head {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  display: flex;
  gap: 0.5rem;
  padding: 0.2rem 0.3rem;
  text-align: left;
  width: 100%;

  &.plain {
    cursor: default;
  }
}

button.change-head:hover {
  background: rgba(255, 255, 255, 0.06);
  border-radius: 4px;
}

.mark {
  flex: none;
  font-size: 0.68rem;
  letter-spacing: 0.04em;
  min-width: 4.5rem;
  text-transform: uppercase;

  &.changed {
    color: #fcd34d;
  }

  &.added {
    color: #bef264;
  }

  &.removed {
    color: #fca5a5;
  }
}

.who {
  flex: none;
}

.keys {
  color: #9ca3af;
  font-family: monospace;
  font-size: 0.72rem;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.change-body {
  padding: 0.2rem 0.3rem 0.5rem 5rem;

  .field + .field {
    margin-top: 0.4rem;
  }
}

.field-key {
  color: #9ca3af;
  font-size: 0.72rem;
}

.sides {
  display: grid;
  gap: 0.4rem;
  grid-template-columns: 1fr 1fr;

  pre {
    background: #0b1220;
    border: 1px solid #374151;
    border-radius: 4px;
    font-size: 0.72rem;
    margin: 0.2rem 0 0;
    max-height: 12rem;
    overflow: auto;
    padding: 0.35rem 0.5rem;
    white-space: pre-wrap;
    word-break: break-word;
  }

  .was {
    border-color: #7a2020;
  }

  .now {
    border-color: #2f5d3a;
  }
}

.import-review {
  background: var(--background-dark);
  border: 1px solid var(--spooky-green);
  border-radius: 8px;
  margin-bottom: 0.9rem;
  outline: none;
  padding: 0.9rem 1rem;

  > header {
    align-items: baseline;
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    margin-bottom: 0.6rem;
  }

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.25em;
    margin: 0;
  }

  .from {
    font-size: 0.8rem;
    opacity: 0.65;
  }

  .note {
    font-size: 0.8rem;
    margin: 0.5rem 0 0;
    max-width: 70ch;
    opacity: 0.75;
  }

  .warn {
    align-items: center;
    color: var(--delete);
    display: flex;
    font-size: 0.8rem;
    gap: 0.4rem;
    margin: 0.5rem 0 0;
  }

  .link {
    background: none;
    border: none;
    color: var(--spooky-green);
    cursor: pointer;
    font-size: 0.8rem;
    padding: 0.35rem 0;
  }
}

/* The count leads, with the replace split trailing it in muted text. */
.count {
  font-size: 0.9rem;
  margin: 0 0 0.6rem;

  .split {
    display: inline-flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    opacity: 0.75;
    padding-left: 0.4rem;

    .added {
      color: var(--spooky-green);
      opacity: 1;
    }

    .dropped {
      color: var(--delete);
      opacity: 1;
    }
  }
}

.types {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
  list-style: none;
  margin: 0;
  padding: 0;

  li {
    background: rgba(255, 255, 255, 0.06);
    border: 1px solid var(--box-border);
    border-radius: 999px;
    font-size: 0.75rem;
    padding: 0.1rem 0.5rem;
    white-space: nowrap;
  }

  .n {
    color: var(--spooky-green);
    font-weight: bold;
    padding-right: 0.3rem;
  }
}

/* One clipped row until asked otherwise, and even then capped with its own
   scroller: a pool export runs to hundreds of cards and would take the page. */
.review-preview {
  margin-top: 0.75rem;

  &.expanded {
    max-height: 45vh;
    overflow-y: auto;
    overscroll-behavior: contain;
  }
}

.review-actions {
  display: flex;
  gap: 0.5rem;
  margin-top: 0.6rem;

  button {
    font-size: 0.85rem;
    padding: 0.35rem 0.9rem;
  }

  .confirm {
    background: var(--spooky-green);
    border: 1px solid var(--spooky-green);
    color: var(--background-dark);

    &:disabled {
      cursor: default;
      opacity: 0.5;
    }
  }

  .cancel {
    background: none;
    border: 1px solid var(--box-border);
    color: var(--title);
  }
}

/* Follows the card down. A def runs well past a screen, and the save button,
   what is unsaved and which card this is are wanted from anywhere in it --
   scrolling back to the top to save was the whole of the trip. Bled out to the
   panel's edges so nothing passes between it and the border on the way by. */
.builder-head {
  align-items: center;
  background: var(--background-dark);
  border-radius: 8px 8px 0 0;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin: -1rem -1rem 0.75rem;
  padding: 1rem 1rem 0.6rem;
  position: sticky;
  top: 0;
  z-index: 4;

  /* Drawn only once it has left its place, so the line means "there is more
     above this" rather than decorating the heading. */
  &.stuck {
    border-bottom: 1px solid rgba(255, 255, 255, 0.12);
  }

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }
}

/* Pushed right on its own, so the title stays hard left whether or not the
   expand button is in front of it — `space-between` shoved it to the middle. */
/* The one button up here that cannot be undone. It wears what the panel's own
   delete wore, since it is the same act moved rather than a new one. */
.builder-actions .delete:hover {
  border-color: var(--delete);
  color: var(--delete);
}

.builder-actions {
  align-items: center;
  display: flex;
  gap: 0.5rem;
  margin-left: auto;
}

/* Said twice, because one of them is easy to miss: beside the heading, and on
   the button that resolves it. */
.unsaved {
  align-items: center;
  color: var(--important);
  display: inline-flex;
  font-family: sans-serif;
  font-size: 0.55em;
  gap: 0.35em;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  vertical-align: middle;
}

.unsaved-dot {
  background: currentColor;
  border-radius: 50%;
  display: inline-block;
  height: 0.5em;
  width: 0.5em;
}

button.save.dirty {
  box-shadow: 0 0 0 2px var(--important);
}

.muted {
  font-size: 0.85rem;
  margin: 0 0 0.75rem;
  opacity: 0.75;
}

.status {
  color: var(--spooky-green);
  font-size: 0.85rem;
}

.error {
  color: var(--delete);
  font-size: 0.85rem;
}

button {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.85rem;
  padding: 0.35rem 0.7rem;

  &:disabled {
    cursor: default;
    opacity: 0.4;
  }
}
</style>
