<script lang="ts" setup>
/* The card builder: your library on the left, the card you are working on to
 * the right. Cards live against your account, so they outlive any one game.
 *
 * In a game you only pick from this library; building and editing happen here,
 * where there is room for it. */
import { computed, nextTick, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import CustomCardForm from '@/arkham/components/debug/CustomCardForm.vue'
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

/* The set being worked on, and the cards in it, in printed order. A card is
 * built into a set, so the builder shows one at a time rather than the whole
 * library at once. */
const activeCards = computed(() => {
  if (!activeSetId.value) return []
  return setCards(activeSetId.value).sort((a, b) =>
    (a.def.meta?.number ?? '').localeCompare(b.def.meta?.number ?? '', undefined, { numeric: true }),
  )
})

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
    chooseActiveSet(set.id)
    startNew()
  } catch (e) {
    console.error(e)
    error.value = 'Could not create that set.'
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
    error.value = 'Could not rename that set.'
  }
}

/* The whole point of a set: changing your mind about an import you just made is
 * one decision, so the count is spelled out rather than left to be discovered. */
async function dropSet(set: LibrarySet) {
  const count = set.cardCount
  const what = count === 1 ? 'its 1 card' : `its ${count} cards`
  if (!confirm(`Delete "${set.name}" and ${count ? what : 'nothing else — it is empty'}?`)) return
  error.value = null
  try {
    const editing = editingCode.value
    await removeSet(set.id)
    if (activeSetId.value === set.id) chooseActiveSet(sets.value[0]?.id ?? null)
    if (editing && !libraryCard(editing)) startNew()
    status.value = `Deleted "${set.name}".`
  } catch (e) {
    console.error(e)
    error.value = 'Could not delete that set.'
  }
}

async function edit(card: CustomCard) {
  editingCode.value = card.def.cardCode
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
    error.value = 'Make a set first — a card has to go in one.'
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
    status.value = 'Saved.'
  } catch (e) {
    console.error(e)
    error.value = 'Could not save the card. Check the raw JSON, if you used any.'
  } finally {
    busy.value = false
  }
}

async function remove(card: CustomCard) {
  if (!confirm(`Delete "${card.def.name.title}" from your library?`)) return
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
  download(setCards(set.id), `${slug(set.name)}.arkhamcard.json`, set)

async function exportSelected() {
  const chosen = activeCards.value.filter((c) => isSelected(c.def.cardCode))
  if (!chosen.length) return
  await download(
    chosen,
    chosen.length === 1 ? `${slug(chosen[0].def.name.title)}.arkhamcard.json` : `custom-cards-${chosen.length}.json`,
  )
}

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
  try {
    const { name, sourceCode, cards: incoming } = await parse(await file.text())
    if (!incoming.length) {
      error.value = 'That file has no cards in it.'
      return
    }

    const replacing = sets.value.find(
      (s) => (sourceCode && s.sourceCode === sourceCode) || s.name === name,
    )
    const prompt = replacing
      ? `Replace "${replacing.name}" (${replacing.cardCount} cards) with the ${incoming.length} in this file?`
      : `Import ${incoming.length} card${incoming.length === 1 ? '' : 's'} as "${name}"?`
    if (!confirm(prompt)) return

    const editing = editingCode.value
    const set = await importSet({ name, sourceCode, cards: incoming })
    chooseActiveSet(set.id)
    if (editing && !libraryCard(editing)) startNew()
    status.value = `Imported ${incoming.length} card${incoming.length === 1 ? '' : 's'} into "${set.name}".`
  } catch (e) {
    console.error(e)
    error.value = 'Could not read that file.'
  }
}

const fileBaseName = (file: File) => file.name.replace(/\.[^.]+$/, '').replace(/\.arkhamcard$/, '')

async function onImport(event: Event) {
  const input = event.target as HTMLInputElement
  const file = input.files?.[0]
  input.value = ''
  if (!file) return
  await importFile(file, async (text) => {
    const { parseCardExport } = await import('@/arkham/customCardLibrary')
    return parseCardExport(text, fileBaseName(file))
  })
}

/* A different source format from `onImport` above: an arkham.build ("Arkham
 * Card Maker") card-pool export, not this app's own `.arkhamcard.json`. Only
 * the simple fields come across -- name, type, class, cost, stats, traits,
 * art -- never ability text, which this app has no field for at all. Cards
 * are coded deterministically from their arkham.build id, so a deck built on
 * arkham.build against these same cards resolves against these rows instead
 * of going missing, and re-importing the same file updates them in place. */
async function onImportArkhamBuild(event: Event) {
  const input = event.target as HTMLInputElement
  const file = input.files?.[0]
  input.value = ''
  if (!file) return
  await importFile(file, async (text) => {
    const { parseArkhamBuildCards, arkhamBuildCardToCustomCard } = await import('@/arkham/arkhamBuildImport')
    const { packName, packCode, cards: rawCards } = parseArkhamBuildCards(text)
    return {
      name: packName ?? fileBaseName(file),
      sourceCode: packCode,
      cards: rawCards.map((raw: any) => arkhamBuildCardToCustomCard(raw, packName)),
    }
  })
}
</script>

<template>
  <div class="page-container">
    <div class="card-builder">
    <aside class="library" :class="{ collapsed: libraryCollapsed }">
      <div class="library-content">
      <div class="library-head">
        <button
          type="button"
          class="library-collapse"
          title="Hide library"
          @click="libraryCollapsed = true"
        >
          «
        </button>
        <h2>Sets</h2>
        <button type="button" class="new-card" :disabled="!activeSetId" @click="startNew">
          + New card
        </button>
      </div>

      <div class="library-tools">
        <label class="tool import">
          <span>Import</span>
          <input type="file" accept="application/json,.json" @change="onImport" />
        </label>
        <label class="tool import">
          <span>Import arkham.build</span>
          <input type="file" accept="application/json,.json" @change="onImportArkhamBuild" />
        </label>
      </div>

      <p v-if="!libraryLoaded" class="muted">Loading…</p>

      <template v-else>
        <ul class="set-list">
          <li v-for="set in sets" :key="set.id" :class="{ active: set.id === activeSetId }">
            <input
              v-if="renamingSetId === set.id"
              v-model="renameDraft"
              class="rename"
              type="text"
              @keydown.enter="commitRename(set)"
              @keydown.esc="renamingSetId = null"
              @blur="commitRename(set)"
            />
            <button v-else type="button" class="set-name" @click="chooseActiveSet(set.id)">
              <span class="name">{{ set.name }}</span>
              <span class="group-count">{{ set.cardCount }}</span>
            </button>
            <div class="row-actions">
              <button type="button" title="Rename this set" @click="startRename(set)">
                <font-awesome-icon icon="pen" />
              </button>
              <button type="button" :title="`Export ${set.name}`" @click="exportSet(set)">
                <font-awesome-icon icon="download" />
              </button>
              <button
                type="button"
                class="delete"
                title="Delete this set and its cards"
                @click="dropSet(set)"
              >
                <font-awesome-icon icon="trash" />
              </button>
            </div>
          </li>
        </ul>

        <form class="new-set" @submit.prevent="addSet">
          <input v-model="newSetName" type="text" placeholder="New set name" @keydown.stop />
          <button type="submit" :disabled="!newSetName.trim()">Add</button>
        </form>

        <template v-if="activeSet">
          <div class="group-head">
            <h3>{{ activeSet.name }}</h3>
            <span class="group-count">{{ activeCards.length }}</span>
            <div class="row-actions">
              <button type="button" :disabled="!activeCards.length" title="Select every card" @click="selectAll">
                <font-awesome-icon icon="check-double" />
              </button>
              <button
                type="button"
                :disabled="!selected.length"
                :title="`Export ${selected.length} selected`"
                @click="exportSelected"
              >
                <font-awesome-icon icon="download" />
              </button>
              <button type="button" :disabled="!selected.length" title="Clear selection" @click="clearSelection">
                <font-awesome-icon icon="times" />
              </button>
            </div>
          </div>

          <p v-if="!activeCards.length" class="muted">
            Nothing in this set yet. Build a card and it lands here.
          </p>
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
                <button type="button" title="Export this card" @click="exportOne(card)">
                  <font-awesome-icon icon="download" />
                </button>
                <button type="button" class="delete" title="Delete this card" @click="remove(card)">
                  <font-awesome-icon icon="trash" />
                </button>
              </div>
            </li>
          </ul>
        </template>
      </template>
      </div>
    </aside>

    <main class="builder">
      <header class="builder-head">
        <button
          v-if="libraryCollapsed"
          type="button"
          class="library-expand"
          title="Show library"
          @click="libraryCollapsed = false"
        >
          » Library
        </button>
        <h2>
          {{ editingCode ? 'Editing card' : 'New card' }}
          <small v-if="activeSet" class="in-set">in {{ activeSet.name }}</small>
        </h2>
        <div class="builder-actions">
          <span v-if="status" class="status">{{ status }}</span>
          <span v-if="error" class="error">{{ error }}</span>
          <button type="button" :disabled="busy || !activeSetId" @click="save">
            {{ editingCode ? 'Save changes' : 'Create card' }}
          </button>
          <button v-if="editingCode" type="button" class="secondary" @click="startNew">New card</button>
        </div>
      </header>

      <p class="experimental">
        <font-awesome-icon icon="flask" />
        Experimental. This feature can stop working, break a game in progress, or be removed at any
        time.
      </p>

      <p v-if="editingCode" class="muted">
        Saving updates every copy of this card — name, traits, art and abilities apply at once. An
        enemy's printed fight, health and evade are copied when it is built, so those only apply to
        copies put into play after the save.
      </p>

      <!-- A card belongs to a set, so there is nothing to build until there is
           one to build it into. -->
      <form v-if="libraryLoaded && !activeSetId" class="first-set" @submit.prevent="addSet">
        <h3>Make a set first</h3>
        <p class="muted">
          Cards are built into a set — the thing you name, export, and can throw away in one go.
        </p>
        <div class="first-set-row">
          <input v-model="newSetName" type="text" placeholder="My Expansion" @keydown.stop />
          <button type="submit" :disabled="!newSetName.trim()">Create set</button>
        </div>
      </form>

      <CustomCardForm v-else ref="form" />
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

.card-builder {
  display: flex;
  gap: 1.5rem;
  align-items: flex-start;
  padding: 1.5rem;
  color: var(--title);

  @media (max-width: 900px) {
    flex-direction: column;
  }
}

.library {
  position: relative;
  flex: 0 0 300px;
  transition: flex-basis 0.18s ease, padding 0.18s ease;

  &.collapsed {
    border: none;
    flex-basis: 0;
    overflow: visible;
    padding: 0;

    .library-content {
      display: none;
    }
  }
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  padding: 1rem;
  max-height: calc(100vh - var(--nav-height) - 3rem);
  overflow: auto;
  position: sticky;
  top: 0;

  @media (max-width: 900px) {
    position: static;
    flex: 1 1 auto;
    width: 100%;
    max-height: none;
  }
}

.library-collapse,
.library-expand {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.85rem;
  padding: 0.15rem 0.4rem;
}

/* Title on the left with the collapse tucked against it, one action on the
 * right — nothing competes for the same corner. */
.library-head {
  align-items: center;
  display: grid;
  grid-template-columns: auto 1fr auto;
  gap: 0.5rem;
  margin-bottom: 0.6rem;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }
}

.new-card {
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.2rem 0.55rem;
  white-space: nowrap;

  &:hover {
    background: rgba(255, 255, 255, 0.14);
  }
}

/* Four evenly sized controls in a fixed 2x2 grid: the panel is narrow, and a
 * wrapping flex row left "Import" stranded on a line of its own. */
.library-tools {
  display: grid;
  gap: 0.3rem;
  grid-template-columns: 1fr 1fr;
  margin-bottom: 0.9rem;
}

.tool {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid var(--box-border);
  border-radius: 4px;
  color: var(--title);
  cursor: pointer;
  font-size: 0.75rem;
  padding: 0.3rem 0.4rem;
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

/* The sets themselves, above the cards of whichever one is open. */
.set-list {
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  list-style: none;
  margin: 0 0 0.5rem;
  padding: 0;

  li {
    align-items: center;
    border: 1px solid transparent;
    border-radius: 6px;
    display: grid;
    gap: 0.35rem;
    grid-template-columns: minmax(0, 1fr) auto;
    padding: 0.25rem 0.35rem;

    &:hover {
      background: rgba(255, 255, 255, 0.04);
    }

    &.active {
      background: rgba(255, 255, 255, 0.07);
      border-color: var(--spooky-green);
    }
  }

  .rename {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-size: 0.85rem;
    min-width: 0;
    padding: 0.15rem 0.3rem;
    width: 100%;
  }
}

.set-name {
  align-items: baseline;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  gap: 0.4rem;
  min-width: 0;
  padding: 0;
  text-align: left;

  .name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
}

.new-set {
  display: grid;
  gap: 0.3rem;
  grid-template-columns: minmax(0, 1fr) auto;
  margin-bottom: 0.9rem;

  input {
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    font-size: 0.8rem;
    min-width: 0;
    padding: 0.25rem 0.4rem;
  }

  button {
    font-size: 0.75rem;
    padding: 0.25rem 0.5rem;
  }
}

.first-set {
  background: rgba(255, 255, 255, 0.03);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  padding: 1.25rem;

  h3 {
    font-family: teutonic, sans-serif;
    font-size: 1.15em;
    margin: 0 0 0.4rem;
  }
}

.first-set-row {
  display: flex;
  gap: 0.5rem;
  max-width: 28rem;

  input {
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid var(--box-border);
    border-radius: 4px;
    color: var(--title);
    flex: 1 1 auto;
    min-width: 0;
    padding: 0.35rem 0.5rem;
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

.library-group h3 {
  font-size: 0.75rem;
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
  padding: 0;

  /* Checkbox, art, text, actions: a fixed grid so the type never collides with
   * the buttons and the row never wraps to two lines. */
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
  justify-content: space-between;
  margin-bottom: 0.75rem;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }
}

.builder-actions {
  align-items: center;
  display: flex;
  gap: 0.5rem;
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
