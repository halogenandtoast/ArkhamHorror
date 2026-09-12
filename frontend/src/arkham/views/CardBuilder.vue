<script lang="ts" setup>
/* The card builder: your library on the left, the card you are working on to
 * the right. Cards live against your account, so they outlive any one game.
 *
 * In a game you only pick from this library; building and editing happen here,
 * where there is room for it. */
import { computed, nextTick, onMounted, onUnmounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useI18n } from 'vue-i18n'
import CustomCardForm from '@/arkham/components/debug/CustomCardForm.vue'
import SegmentedToggle from '@/components/SegmentedToggle.vue'
import { stripCardCodePrefix } from '@/arkham/customCards'
import {
  mintCustomCardCode,
  renderCardPlaceholder,
  type CustomCard,
} from '@/arkham/customCards'
import {
  createSet,
  exportCards,
  importSet,
  libraryCard,
  libraryCards,
  libraryLoaded,
  librarySet,
  librarySets,
  loadLibrary,
  removeFromLibrary,
  removeSet,
  renameSet,
  saveToLibrary,
  setCards,
  type LibrarySet,
} from '@/arkham/customCardLibrary'

const { t } = useI18n()
const K = 'customCardSets.'

const form = ref<InstanceType<typeof CustomCardForm> | null>(null)
const editingCode = ref<string | null>(null)
const selected = ref<string[]>([])
const busy = ref(false)
const libraryCollapsed = ref(false)
const status = ref<string | null>(null)
const error = ref<string | null>(null)

/* Which set new cards are written into. Remembered across visits because it is
 * the thing you are working on, not a filter you re-pick every time. */
const ACTIVE_SET_KEY = 'arkham:card-builder:active-set'
const activeSetId = ref<string | null>(null)
const newSetName = ref('')
const renamingSetId = ref<string | null>(null)
const renameDraft = ref('')

const route = useRoute()
const router = useRouter()

/* ?card=<code> opens the builder on that card: a deep link from a game ("Edit
 * custom card" on an asset), and the way one card links to another — a
 * signature to the investigator whose it is. Watched rather than read once, so
 * a link followed while already here still lands. */
async function openFromRoute() {
  await loadLibrary()
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

onMounted(openFromRoute)
watch(() => route.query.card, openFromRoute)

const cards = computed(() => libraryCards())
const sets = computed(() => librarySets())
const activeSet = computed(() => librarySet(activeSetId.value))

/* Printed order -- the card number, which is the order the set's author put
 * them in. `setCards` hands them back most-recently-edited first, which is the
 * wrong order anywhere a whole set is on show. */
const inPrintedOrder = (setId: string) =>
  setCards(setId).sort((a, b) =>
    (a.def.meta?.number ?? '').localeCompare(b.def.meta?.number ?? '', undefined, { numeric: true }),
  )

/* The set being worked on, and the cards in it. A card is built into a set, so
 * the builder shows one at a time rather than the whole library at once. */
const activeCards = computed(() =>
  activeSetId.value ? inPrintedOrder(activeSetId.value) : [],
)

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
  startNew()
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

const setQuery = ref('')
const setOrder = ref<'name' | 'recent'>('name')
const setOrderOptions = computed(() => [
  { value: 'name' as const, label: t(`${K}orderAlphabetical`) },
  { value: 'recent' as const, label: t(`${K}orderRecent`) },
])

/* Saving a card does not touch its set's row -- that only moves on a rename or
 * an import -- so how recently a set was worked on is the newest card in it. */
const lastTouched = (set: LibrarySet) =>
  setCards(set.id).reduce((newest, c) => (c.updatedAt > newest ? c.updatedAt : newest), set.updatedAt)

const matches = (text: string | null | undefined, query: string) =>
  !!text && text.toLowerCase().includes(query)

/* The cards in a set that answer the filter; all of them when there is none. */
function matchingCards(setId: string) {
  const query = setQuery.value.trim().toLowerCase()
  const cards = inPrintedOrder(setId)
  if (!query) return cards
  return cards.filter((c) => matches(c.def.name.title, query) || matches(c.def.name.subtitle, query))
}

/* A set stays in the list when its own name matches or one of its cards does,
 * so searching for a card finds the set holding it. */
const visibleSets = computed(() => {
  const query = setQuery.value.trim().toLowerCase()
  const shown = query
    ? sets.value.filter((s) => matches(s.name, query) || matchingCards(s.id).length > 0)
    : [...sets.value]
  // `sets` is already alphabetical, so only the other order has to be applied.
  if (setOrder.value === 'recent') {
    return shown.sort((a, b) => lastTouched(b).localeCompare(lastTouched(a)))
  }
  return shown
})

/* Which sets have cards the preview row could not fit. The row is clipped by
 * CSS, so the browser is the only thing that knows how many fit -- asked here
 * rather than guessed at from widths and gaps -- and "View all" is pointless on
 * a set that is already showing everything. */
const setList = ref<HTMLElement | null>(null)
const overflowingSets = ref<string[]>([])

function measurePreviews() {
  const root = setList.value
  if (!root) return
  overflowingSets.value = [...root.querySelectorAll<HTMLElement>('[data-set-id]')]
    .filter((el) => el.scrollHeight > el.clientHeight + 1)
    .map((el) => el.dataset.setId!)
}

let previewObserver: ResizeObserver | null = null

watch(setList, (el) => {
  previewObserver?.disconnect()
  if (!el) return
  previewObserver ??= new ResizeObserver(measurePreviews)
  previewObserver.observe(el)
})

// A row added or dropped changes the list's height and the observer catches it;
// a change within the same height (the filter, a deleted card) does not.
watch([visibleSets, setQuery], () => nextTick(measurePreviews))

onUnmounted(() => previewObserver?.disconnect())

const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)
const isSelected = (code: string) => selected.value.includes(code)

function toggleSelected(code: string) {
  const index = selected.value.indexOf(code)
  if (index === -1) selected.value.push(code)
  else selected.value.splice(index, 1)
}

const selectAll = () => (selected.value = activeCards.value.map((c) => c.def.cardCode))
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
  renameDraft.value = set.name
}

async function commitRename(set: LibrarySet) {
  const name = renameDraft.value.trim()
  renamingSetId.value = null
  if (!name || name === set.name) return
  error.value = null
  try {
    await renameSet(set.id, name)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}setRenameFailed`)
  }
}

/* The whole point of a set: changing your mind about an import you just made is
 * one decision, so the count is spelled out rather than left to be discovered. */
async function dropSet(set: LibrarySet) {
  const count = set.cardCount
  const what = count ? t(`${K}deleteSetCards`, count) : t(`${K}deleteSetEmpty`)
  if (!confirm(t(`${K}confirmDeleteSet`, { name: set.name, what }))) return
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
  syncRoute(card.def.cardCode)
}

function startNew() {
  editingCode.value = null
  status.value = null
  error.value = null
  form.value?.reset()
  syncRoute(null)
}

/* The url names the card being edited, so a refresh comes back to it and the
 * link is worth sharing. Replaced rather than pushed: opening one card after
 * another should not fill up the back button. */
function syncRoute(cardCode: string | null) {
  const current = route.query.card
  if (cardCode === null) {
    if (current === undefined) return
    router.replace({ name: 'CardBuilder', query: {} })
    return
  }
  if (typeof current === 'string' && stripCardCodePrefix(current) === stripCardCodePrefix(cardCode)) {
    return
  }
  router.replace({ name: 'CardBuilder', query: { card: cardCode } })
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
    status.value = t(`${K}saved`)
  } catch (e) {
    console.error(e)
    error.value = t(`${K}saveFailed`)
  } finally {
    busy.value = false
  }
}

async function remove(card: CustomCard) {
  if (!confirm(t(`${K}confirmDeleteCard`, { name: card.def.name.title }))) return
  await removeFromLibrary(card.def.cardCode)
  selected.value = selected.value.filter((c) => c !== card.def.cardCode)
  if (editingCode.value === card.def.cardCode) startNew()
}

// ---------------------------------------------------------------- export ---

async function download(cards: CustomCard[], filename: string, set?: LibrarySet) {
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

async function exportSelected() {
  const chosen = activeCards.value.filter((c) => isSelected(c.def.cardCode))
  if (!chosen.length) return
  await download(
    chosen,
    chosen.length === 1 ? `${slug(chosen[0].def.name.title)}.arkhamcard.json` : `custom-cards-${chosen.length}.json`,
  )
}

/* How many investigator minis the last import had to cut for itself. Set by the
 * arkham.build path, cleared by every import, and only ever read to say so. */
const portraitsCut = ref(0)

/* Everything imported arrives as a set, replacing one of the same name rather
 * than merging into it. A file that names no set falls back to what the cards
 * claim, and failing that to the file's own name. */
async function importFile(file: File, parse: (text: string) => Promise<{
  name: string
  sourceCode: string | null
  cards: CustomCard[]
}>) {
  error.value = null
  status.value = null
  portraitsCut.value = 0
  try {
    const { name, sourceCode, cards: incoming } = await parse(await file.text())
    if (!incoming.length) {
      error.value = t(`${K}fileHasNoCards`)
      return
    }

    const replacing = sets.value.find(
      (s) => (sourceCode && s.sourceCode === sourceCode) || s.name === name,
    )
    const prompt = replacing
      ? t(`${K}confirmReplace`, {
          name: replacing.name,
          existing: t(`${K}cardCount`, replacing.cardCount),
          incoming: incoming.length,
        })
      : t(`${K}confirmImport`, { count: t(`${K}cardCount`, incoming.length), name })
    if (!confirm(prompt)) return

    const editing = editingCode.value
    const set = await importSet({ name, sourceCode, cards: incoming })
    chooseActiveSet(set.id)
    if (editing && !libraryCard(editing)) startNew()
    const cut = portraitsCut.value
    status.value = t(`${K}imported`, {
      count: t(`${K}cardCount`, incoming.length),
      name: set.name,
      minis: cut ? ` ${t(`${K}minisCut`, cut)}` : '',
    })
  } catch (e) {
    console.error(e)
    error.value = t(`${K}importFailed`)
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
async function onImport(event: Event) {
  const input = event.target as HTMLInputElement
  const file = input.files?.[0]
  input.value = ''
  if (!file) return
  await importFile(file, async (text) => {
    if (!isArkhamBuildExport(JSON.parse(text))) {
      const { parseCardExport } = await import('@/arkham/customCardLibrary')
      return parseCardExport(text, fileBaseName(file))
    }
    const { parseArkhamBuildCards, arkhamBuildCardToCustomCard, attachInvestigatorPortraits } =
      await import('@/arkham/arkhamBuildImport')
    const { packName, packCode, cards: rawCards } = parseArkhamBuildCards(text)
    const cards = rawCards.map((raw: any) => arkhamBuildCardToCustomCard(raw, packName))
    /* There is no mini in the export, so an investigator's is cut out of its own
     * card face. Said out loud in the status line, because it is a guess at
     * where the art sits and the author may want to replace it. */
    portraitsCut.value = await attachInvestigatorPortraits(cards)
    return { name: packName ?? fileBaseName(file), sourceCode: packCode, cards }
  })
}
</script>

<template>
  <div class="page-container">

  <!-- Your sets, full width. The editor is one card at a time, so it has no
       room to show a set; here a set can open up and show its cards. -->
  <section v-if="!inSet" class="sets-page">
    <header class="sets-head">
      <h1>{{ t(`${K}title`) }}</h1>
      <div class="sets-tools">
        <form class="new-set" @submit.prevent="addSet">
          <input v-model="newSetName" type="text" :placeholder="t(`${K}newSetName`)" @keydown.stop />
          <button type="submit" :disabled="!newSetName.trim()">{{ t(`${K}addSet`) }}</button>
        </form>
        <label
          class="tool import"
          v-tooltip="t(`${K}importTooltip`)"
        >
          <span>{{ t(`${K}import`) }}</span>
          <input type="file" accept="application/json,.json" @change="onImport" />
        </label>
      </div>
    </header>

    <p class="experimental">
      <font-awesome-icon icon="flask" />
      {{ t(`${K}experimental`) }}
    </p>

    <p v-if="status" class="status">{{ status }}</p>
    <p v-if="error" class="error">{{ error }}</p>

    <p v-if="!libraryLoaded" class="muted">{{ t(`${K}loading`) }}</p>

    <!-- Nothing to show, so say what the thing is instead. -->
    <section v-else-if="!sets.length" class="empty">
      <h2>{{ t(`${K}emptyTitle`) }}</h2>
      <p class="lede">{{ t(`${K}emptyLede`) }}</p>
    </section>

    <template v-else>
      <div class="sets-browse">
        <div class="set-filter">
          <font-awesome-icon icon="search" />
          <input
            v-model="setQuery"
            type="search"
            :placeholder="t(`${K}filterPlaceholder`)"
            :aria-label="t(`${K}filterLabel`)"
            @keydown.stop
          />
          <button
            v-if="setQuery"
            type="button"
            class="clear"
            v-tooltip="t(`${K}clearFilter`)" :aria-label="t(`${K}clearFilter`)"
            @click="setQuery = ''"
          >
            <font-awesome-icon icon="times" />
          </button>
        </div>
        <SegmentedToggle
          v-model="setOrder"
          class="set-order"
          :options="setOrderOptions"
          :label="t(`${K}orderLabel`)"
        />
      </div>

      <p v-if="!visibleSets.length" class="muted empty">
        {{ t(`${K}noMatches`, { query: setQuery.trim() }) }}
      </p>

      <ul v-else ref="setList" class="set-cards">
      <li v-for="set in visibleSets" :key="set.id">
        <div class="set-row">
          <input
            v-if="renamingSetId === set.id"
            v-model="renameDraft"
            class="rename"
            type="text"
            @keydown.enter="commitRename(set)"
            @keydown.esc="renamingSetId = null"
            @blur="commitRename(set)"
          />
          <button v-else type="button" class="set-open" @click="openSet(set.id)">
            <span class="name">{{ set.name }}</span>
            <span class="group-count">{{ t(`${K}cardCount`, set.cardCount) }}</span>
          </button>

          <div class="row-actions">
            <button type="button" v-tooltip="t(`${K}renameSet`)" :aria-label="t(`${K}renameSet`)" @click="startRename(set)">
              <font-awesome-icon icon="pen" />
            </button>
            <button
              type="button"
              v-tooltip="t(`${K}exportSet`, { name: set.name })"
              :aria-label="t(`${K}exportSet`, { name: set.name })"
              @click="exportSet(set)"
            >
              <font-awesome-icon icon="download" />
            </button>
            <button
              type="button"
              class="delete"
              v-tooltip="t(`${K}deleteSet`)" :aria-label="t(`${K}deleteSet`)"
              @click="dropSet(set)"
            >
              <font-awesome-icon icon="trash" />
            </button>
          </div>
        </div>

        <!-- As many cards as fit on one row, and no more: the grid's auto-fill
             decides how many that is, and the row below it is clipped. -->
        <div class="set-preview">
          <p v-if="!matchingCards(set.id).length" class="muted">
            {{ setQuery.trim() ? t(`${K}noCardMatches`) : t(`${K}emptySet`) }}
          </p>
          <div v-else class="set-gallery" :data-set-id="set.id">
            <button
              v-for="card in matchingCards(set.id)"
              :key="card.def.cardCode"
              type="button"
              class="gallery-card"
              v-tooltip="t(`${K}editCard`, { name: card.def.name.title })"
              @click="edit(card)"
            >
              <img :src="cardArt(card)" :data-image-id="card.def.cardCode" alt="" />
              <span class="name">{{ card.def.name.title }}</span>
            </button>
          </div>

          <button
            v-if="overflowingSets.includes(set.id)"
            type="button"
            class="view-all"
            @click="openSet(set.id)"
          >
            {{ t(`${K}viewAll`, { count: set.cardCount }) }} →
          </button>
        </div>
      </li>
      </ul>
    </template>
  </section>

  <div v-else class="card-builder">
    <aside class="library" :class="{ collapsed: libraryCollapsed }">
      <div class="library-content">
      <div class="library-head">
        <h2 class="panel-title" :title="activeSet?.name">{{ activeSet?.name }}</h2>
      </div>

      <button type="button" class="back-to-sets" @click="browsingSets = true">
        ← {{ t(`${K}backToSets`) }}
      </button>

      <div class="group-head">
        <h3>{{ t(`${K}cardCount`, activeCards.length) }}</h3>
        <div class="row-actions">
          <button
            type="button"
            :disabled="!activeCards.length"
            v-tooltip="t(`${K}selectAll`)"
            :aria-label="t(`${K}selectAll`)"
            @click="selectAll"
          >
            <font-awesome-icon icon="check-double" />
          </button>
          <button
            type="button"
            :disabled="!selected.length"
            v-tooltip="t(`${K}exportSelected`, { count: selected.length })"
            :aria-label="t(`${K}exportSelected`, { count: selected.length })"
            @click="exportSelected"
          >
            <font-awesome-icon icon="download" />
          </button>
          <button
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

      <button type="button" class="new-card" @click="startNew">+ {{ t(`${K}newCard`) }}</button>

      <p v-if="!activeCards.length" class="muted">{{ t(`${K}emptySetPanel`) }}</p>
      <ul v-else class="library-list">
        <li
          v-for="card in activeCards"
          :key="card.def.cardCode"
          :class="{ editing: editingCode === card.def.cardCode }"
        >
          <input type="checkbox" :checked="isSelected(card.def.cardCode)" @change="toggleSelected(card.def.cardCode)" />
          <button type="button" class="library-card" @click="edit(card)">
            <img :src="cardArt(card)" :data-image-id="card.def.cardCode" alt="" />
            <span class="text">
              <span class="name">{{ card.def.name.title }}</span>
              <small>{{ card.def.cardType.replace(/Type$/, '') }}</small>
            </span>
          </button>
          <div class="row-actions">
            <button type="button" v-tooltip="t(`${K}exportCard`)" :aria-label="t(`${K}exportCard`)" @click="exportOne(card)">
              <font-awesome-icon icon="download" />
            </button>
            <button type="button" class="delete" v-tooltip="t(`${K}deleteCard`)" :aria-label="t(`${K}deleteCard`)" @click="remove(card)">
              <font-awesome-icon icon="trash" />
            </button>
          </div>
        </li>
      </ul>
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
      <header class="builder-head">
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
          {{ editingCode ? t(`${K}editingCard`) : t(`${K}newCard`) }}
          <small v-if="activeSet" class="in-set">{{ t(`${K}inSet`, { name: activeSet.name }) }}</small>
        </h2>
        <div class="builder-actions">
          <span v-if="status" class="status">{{ status }}</span>
          <span v-if="error" class="error">{{ error }}</span>
          <button type="button" :disabled="busy" @click="save">
            {{ editingCode ? t(`${K}saveChanges`) : t(`${K}createCard`) }}
          </button>
          <!-- Only worth a slot up here while the panel's own "+ New card" is out
               of reach. -->
          <button
            v-if="libraryCollapsed && editingCode"
            type="button"
            class="secondary"
            @click="startNew"
          >
            + {{ t(`${K}newCard`) }}
          </button>
        </div>
      </header>

      <p v-if="editingCode" class="muted">{{ t(`${K}saveNote`) }}</p>

      <CustomCardForm ref="form" />
    </main>
  </div>
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
.sets-page {
  color: var(--title);
  margin: 0 auto;
  max-width: 1100px;
  padding: 1.5rem;
}

.sets-head {
  align-items: flex-end;
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
  justify-content: space-between;
  margin-bottom: 0.9rem;

  h1 {
    font-family: teutonic, sans-serif;
    font-size: 1.7em;
    margin: 0;
  }
}

.sets-tools {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.sets-browse {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  justify-content: flex-end;
  margin-bottom: 0.75rem;
}

.set-filter {
  align-items: center;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  color: var(--title);
  display: flex;
  flex: 0 1 20rem;
  gap: 0.45rem;
  padding: 0.35rem 0.55rem;
  transition: border-color 0.15s ease;

  &:focus-within {
    border-color: var(--spooky-green);
  }

  > svg {
    flex: none;
    font-size: 0.8rem;
    opacity: 0.5;
  }

  input {
    background: none;
    border: none;
    color: var(--title);
    flex: 1 1 auto;
    font-size: 0.85rem;
    min-width: 0;
    outline: none;

    /* The platform's own clear button, which does not match anything else. */
    &::-webkit-search-cancel-button {
      display: none;
    }
  }

  .clear {
    background: none;
    border: none;
    color: var(--title);
    cursor: pointer;
    flex: none;
    font-size: 0.7rem;
    opacity: 0.5;
    padding: 0;

    &:hover {
      opacity: 1;
    }
  }
}

.set-order {
  flex: none;
  width: 11rem;
}

.empty {
  border: 1px dashed var(--box-border);
  border-radius: 8px;
  margin: 0;
  max-width: 46rem;
  padding: 1.25rem 1.5rem;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0 0 0.5rem;
  }

  .lede {
    margin: 0;
    max-width: 62ch;
    opacity: 0.8;
  }
}

.set-cards {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  list-style: none;
  margin: 0;
  padding: 0;

  > li {
    background: var(--background-dark);
    border: 1px solid var(--box-border);
    border-radius: 8px;
  }
}

.set-row {
  align-items: center;
  display: grid;
  gap: 0.5rem;
  grid-template-columns: minmax(0, 1fr) auto;
  padding: 0.5rem 0.75rem;

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
}

.set-open {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  gap: 0.6rem;
  min-width: 0;
  padding: 0;
  text-align: left;

  .name {
    font-family: teutonic, sans-serif;
    font-size: 1.15em;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  &:hover .name {
    color: var(--spooky-green);
  }
}

/* A single row of cards per set, as a preview. `auto-fill` works out how many
   whole cards fit the column, so the overflow row is clipped on a card edge
   rather than through one. */
.set-preview {
  --preview-card: 110px;
  --preview-card-height: 190px;

  align-items: center;
  border-top: 1px solid var(--box-border);
  display: flex;
  gap: 0.75rem;
  padding: 0.75rem;

  .muted {
    margin: 0;
  }
}

.set-gallery {
  display: grid;
  flex: 1 1 auto;
  gap: 0.75rem;
  grid-auto-rows: var(--preview-card-height);
  grid-template-columns: repeat(auto-fill, var(--preview-card));
  max-height: var(--preview-card-height);
  min-width: 0;
  overflow: hidden;
}

.gallery-card {
  background: none;
  border: 1px solid transparent;
  border-radius: 6px;
  color: inherit;
  cursor: pointer;
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  height: var(--preview-card-height);
  padding: 0.3rem;
  width: var(--preview-card);

  img {
    border-radius: 3px;
    /* A fixed box so the row has one height whatever shape the card is --
       locations and acts are landscape. `drop-shadow` rather than `box-shadow`
       because the shadow has to follow the letterboxed picture, not the box. */
    filter: drop-shadow(1px 1px 2px rgba(0, 0, 0, 0.8));
    height: 156px;
    object-fit: contain;
    width: 100%;
  }

  .name {
    font-size: 0.72rem;
    line-height: 1.2;
    opacity: 0.8;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  &:hover {
    background: rgba(255, 255, 255, 0.05);
    border-color: var(--spooky-green);
  }
}

.view-all {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  flex: none;
  font-size: 0.8rem;
  padding: 0.4rem 0.7rem;
  white-space: nowrap;

  &:hover {
    background: rgba(255, 255, 255, 0.12);
    border-color: var(--spooky-green);
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
  flex: 0 0 300px;
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
  /* The panel's 300px less its padding. Fixed, so the text does not re-wrap
     while the panel is sliding shut. */
  width: 268px;

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

.tool {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.35rem 0.7rem;
  text-align: center;

  &:hover:not(:disabled) {
    background: rgba(255, 255, 255, 0.12);
  }

  &:disabled {
    cursor: default;
    opacity: 0.4;
  }
}

.import {
  input {
    display: none;
  }
}

.new-set {
  display: flex;
  gap: 0.4rem;

  input {
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-size: 0.85rem;
    min-width: 0;
    padding: 0.35rem 0.5rem;
    width: 12rem;
  }

  button {
    font-size: 0.8rem;
    padding: 0.35rem 0.7rem;
  }
}

.in-set {
  font-family: sans-serif;
  font-size: 0.6em;
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

  /* Checkbox, card, actions: a fixed grid so the type never collides with the
   * buttons and the row never wraps to two lines. Two tracks for three children
   * put the actions on a second row of their own. */
  li {
    align-items: center;
    border: 1px solid transparent;
    border-radius: 6px;
    display: grid;
    gap: 0.5rem;
    grid-template-columns: auto minmax(0, 1fr) auto;
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
  grid-template-columns: 34px minmax(0, 1fr);
  min-width: 0;
  padding: 0;
  text-align: left;

  img {
    border-radius: 3px;
    height: 46px;
    object-fit: cover;
    width: 34px;
  }

  /* Name over type rather than beside it: the name gets the whole width to
   * ellipsize into. */
  .text {
    display: flex;
    flex-direction: column;
    min-width: 0;
  }

  .name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  small {
    font-size: 0.7rem;
    opacity: 0.55;
  }
}

.row-actions {
  display: flex;
  gap: 0.1rem;
}

.row-actions button {
  background: none;
  border: none;
  border-radius: 3px;
  color: inherit;
  cursor: pointer;
  font-size: 0.75rem;
  line-height: 1;
  opacity: 0.5;
  padding: 0.25rem 0.35rem;

  &:hover {
    background: rgba(255, 255, 255, 0.1);
    opacity: 1;
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

.experimental {
  align-items: center;
  background: rgba(200, 60, 60, 0.1);
  border: 1px solid var(--delete);
  border-radius: 6px;
  color: var(--delete);
  display: flex;
  font-size: 0.8rem;
  gap: 0.5rem;
  margin: 0 0 0.9rem;
  padding: 0.45rem 0.65rem;
}

.builder-head {
  align-items: center;
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-bottom: 0.75rem;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }
}

/* Pushed right on its own, so the title stays hard left whether or not the
   expand button is in front of it — `space-between` shoved it to the middle. */
.builder-actions {
  align-items: center;
  display: flex;
  gap: 0.5rem;
  margin-left: auto;
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

/* Inline beside the save button in the editor, on their own line on the sets
   page — the default paragraph margins are wrong for the second case. */
p.status,
p.error {
  margin: 0 0 0.75rem;
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
