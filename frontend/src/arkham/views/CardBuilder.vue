<script lang="ts" setup>
/* The card builder: your library on the left, the card you are working on to
 * the right. Cards live against your account, so they outlive any one game.
 *
 * In a game you only pick from this library; building and editing happen here,
 * where there is room for it. */
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import CustomCardForm from '@/arkham/components/debug/CustomCardForm.vue'
import { stripCardCodePrefix } from '@/arkham/customCards'
import {
  mintCustomCardCode,
  registerCustomCards,
  renderCardPlaceholder,
  type CustomCard,
} from '@/arkham/customCards'
import {
  exportCards,
  importLibraryCards,
  libraryCards,
  libraryLoaded,
  loadLibrary,
  removeFromLibrary,
  saveToLibrary,
} from '@/arkham/customCardLibrary'

const form = ref<InstanceType<typeof CustomCardForm> | null>(null)
const editingCode = ref<string | null>(null)
const selected = ref<string[]>([])
const busy = ref(false)
const libraryCollapsed = ref(false)
const status = ref<string | null>(null)
const error = ref<string | null>(null)

const route = useRoute()
const router = useRouter()

/* ?card=<code> opens the builder on that card: a deep link from a game ("Edit
 * custom card" on an asset), and the way one card links to another — a
 * signature to the investigator whose it is. Watched rather than read once, so
 * a link followed while already here still lands. */
async function openFromRoute() {
  await loadLibrary()
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

/* Cards are grouped by the set they name, so a batch built together stays
 * together the way the card browser groups an expansion. */
const grouped = computed(() => {
  const groups = new Map<string, typeof cards.value>()
  for (const card of cards.value) {
    const set = card.def.meta?.set?.trim() || 'Ungrouped'
    if (!groups.has(set)) groups.set(set, [])
    groups.get(set)!.push(card)
  }
  for (const list of groups.values()) {
    list.sort((a, b) => (a.def.meta?.number ?? '').localeCompare(b.def.meta?.number ?? '', undefined, { numeric: true }))
  }
  return [...groups.entries()].sort(([a], [b]) => (a === 'Ungrouped' ? 1 : b === 'Ungrouped' ? -1 : a.localeCompare(b)))
})
const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)
const isSelected = (code: string) => selected.value.includes(code)

function toggleSelected(code: string) {
  const index = selected.value.indexOf(code)
  if (index === -1) selected.value.push(code)
  else selected.value.splice(index, 1)
}

const selectAll = () => (selected.value = cards.value.map((c) => c.def.cardCode))
const clearSelection = () => (selected.value = [])

/* A set is how a batch built together is kept together, so it is also the unit
 * you hand to someone else — and the unit you select. */
const setFullySelected = (setCards: CustomCard[]) =>
  setCards.length > 0 && setCards.every((c) => isSelected(c.def.cardCode))

function toggleSet(setCards: CustomCard[]) {
  const codes = setCards.map((c) => c.def.cardCode)
  if (setFullySelected(setCards)) {
    selected.value = selected.value.filter((code) => !codes.includes(code))
  } else {
    selected.value = [...new Set([...selected.value, ...codes])]
  }
}

async function edit(card: CustomCard) {
  editingCode.value = card.def.cardCode
  status.value = null
  error.value = null
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
  busy.value = true
  error.value = null
  status.value = null

  try {
    // Editing keeps the card's code, so the save replaces it everywhere rather
    // than leaving a second copy behind.
    const card = form.value?.buildCustomCard(editingCode.value ?? mintCustomCardCode())
    if (!card) return
    await saveToLibrary(card)
    registerCustomCards([card])
    editingCode.value = card.def.cardCode
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

async function download(cards: CustomCard[], filename: string) {
  // The art is fetched and inlined, so this waits on the network.
  const blob = new Blob([JSON.stringify(await exportCards(cards), null, 2)], {
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

const exportSet = (set: string, setCards: CustomCard[]) =>
  download(setCards, `${slug(set)}.arkhamcard.json`)

async function exportSelected() {
  const chosen = cards.value.filter((c) => isSelected(c.def.cardCode))
  if (!chosen.length) return
  await download(
    chosen,
    chosen.length === 1 ? `${slug(chosen[0].def.name.title)}.arkhamcard.json` : `custom-cards-${chosen.length}.json`,
  )
}

async function onImport(event: Event) {
  const input = event.target as HTMLInputElement
  const file = input.files?.[0]
  input.value = ''
  if (!file) return

  error.value = null
  status.value = null
  try {
    const { parseCardExport } = await import('@/arkham/customCardLibrary')
    const imported = parseCardExport(await file.text())
    if (!imported.length) {
      error.value = 'That file has no cards in it.'
      return
    }
    const saved = await importLibraryCards(imported)
    registerCustomCards(saved)
    status.value = `Imported ${saved.length} card${saved.length === 1 ? '' : 's'}.`
  } catch (e) {
    console.error(e)
    error.value = 'Could not read that file.'
  }
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
        <h2>Library</h2>
        <button type="button" class="new-card" @click="startNew">+ New</button>
      </div>

      <div class="library-tools">
        <label class="tool import">
          <span>Import</span>
          <input type="file" accept="application/json,.json" @change="onImport" />
        </label>
        <button type="button" class="tool" :disabled="!selected.length" @click="exportSelected">
          Export{{ selected.length ? ` (${selected.length})` : '' }}
        </button>
        <button type="button" class="tool" :disabled="!cards.length" @click="selectAll">
          Select all
        </button>
        <button type="button" class="tool" :disabled="!selected.length" @click="clearSelection">
          Clear
        </button>
      </div>

      <p v-if="!libraryLoaded" class="muted">Loading…</p>
      <p v-else-if="!cards.length" class="muted">
        No cards yet. Build one and it will be waiting here next time.
      </p>

      <template v-else>
        <div v-for="[set, setCards] in grouped" :key="set" class="library-group">
          <div class="group-head">
            <h3>{{ set }}</h3>
            <span class="group-count">{{ setCards.length }}</span>
            <div class="row-actions">
              <button
                type="button"
                :class="{ on: setFullySelected(setCards) }"
                :title="setFullySelected(setCards) ? `Deselect ${set}` : `Select every card in ${set}`"
                @click="toggleSet(setCards)"
              >
                <font-awesome-icon icon="check-double" />
              </button>
              <button type="button" :title="`Export ${set}`" @click="exportSet(set, setCards)">
                <font-awesome-icon icon="download" />
              </button>
            </div>
          </div>
          <ul class="library-list">
            <li
              v-for="card in setCards"
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
        </div>
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
        <h2>{{ editingCode ? 'Editing card' : 'New card' }}</h2>
        <div class="builder-actions">
          <span v-if="status" class="status">{{ status }}</span>
          <span v-if="error" class="error">{{ error }}</span>
          <button type="button" :disabled="busy" @click="save">
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
