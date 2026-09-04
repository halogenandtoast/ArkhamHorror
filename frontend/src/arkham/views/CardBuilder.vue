<script lang="ts" setup>
/* The card builder: your library on the left, the card you are working on to
 * the right. Cards live against your account, so they outlive any one game.
 *
 * In a game you only pick from this library; building and editing happen here,
 * where there is room for it. */
import { computed, onMounted, ref } from 'vue'
import { useRoute } from 'vue-router'
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
const status = ref<string | null>(null)
const error = ref<string | null>(null)

const route = useRoute()

/* Deep link from a card in a game ("Edit custom card" on an asset), so the page
 * opens on the card you were looking at. */
onMounted(async () => {
  await loadLibrary()
  const wanted = route.query.card
  if (typeof wanted !== 'string') return
  const card = cards.value.find((c) => c.def.cardCode === stripCardCodePrefix(wanted))
  if (card) await edit(card)
})

const cards = computed(() => libraryCards())
const cardArt = (card: CustomCard) => card.art ?? renderCardPlaceholder(card.def)
const isSelected = (code: string) => selected.value.includes(code)

function toggleSelected(code: string) {
  const index = selected.value.indexOf(code)
  if (index === -1) selected.value.push(code)
  else selected.value.splice(index, 1)
}

const selectAll = () => (selected.value = cards.value.map((c) => c.def.cardCode))
const clearSelection = () => (selected.value = [])

async function edit(card: CustomCard) {
  editingCode.value = card.def.cardCode
  status.value = null
  error.value = null
  await form.value?.loadCard(card)
}

function startNew() {
  editingCode.value = null
  status.value = null
  error.value = null
  form.value?.reset()
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

function download(cards: CustomCard[], filename: string) {
  const blob = new Blob([JSON.stringify(exportCards(cards), null, 2)], { type: 'application/json' })
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = filename
  link.click()
  URL.revokeObjectURL(url)
}

const slug = (text: string) => text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'card'

const exportOne = (card: CustomCard) => download([card], `${slug(card.def.name.title)}.arkhamcard.json`)

function exportSelected() {
  const chosen = cards.value.filter((c) => isSelected(c.def.cardCode))
  if (!chosen.length) return
  download(
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
  <div class="card-builder">
    <aside class="library">
      <div class="library-head">
        <h2>Library</h2>
        <button type="button" @click="startNew">+ New card</button>
      </div>

      <div class="library-tools">
        <label class="import">
          Import
          <input type="file" accept="application/json,.json" @change="onImport" />
        </label>
        <button type="button" :disabled="!selected.length" @click="exportSelected">
          Export{{ selected.length ? ` (${selected.length})` : '' }}
        </button>
        <button type="button" :disabled="!cards.length" @click="selectAll">All</button>
        <button type="button" :disabled="!selected.length" @click="clearSelection">None</button>
      </div>

      <p v-if="!libraryLoaded" class="muted">Loading…</p>
      <p v-else-if="!cards.length" class="muted">
        No cards yet. Build one and it will be waiting here next time.
      </p>

      <ul v-else class="library-list">
        <li
          v-for="card in cards"
          :key="card.def.cardCode"
          :class="{ editing: editingCode === card.def.cardCode }"
        >
          <input type="checkbox" :checked="isSelected(card.def.cardCode)" @change="toggleSelected(card.def.cardCode)" />
          <button type="button" class="library-card" @click="edit(card)">
            <img :src="cardArt(card)" :data-image-id="card.def.cardCode" alt="" />
            <span class="name">{{ card.def.name.title }}</span>
            <small>{{ card.def.cardType.replace(/Type$/, '') }}</small>
          </button>
          <div class="row-actions">
            <button type="button" title="Export this card" @click="exportOne(card)">⭳</button>
            <button type="button" title="Delete this card" @click="remove(card)">×</button>
          </div>
        </li>
      </ul>
    </aside>

    <main class="builder">
      <header class="builder-head">
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

      <p v-if="editingCode" class="muted">
        Saving updates every copy of this card — name, traits, art and abilities apply at once. An
        enemy's printed fight, health and evade are copied when it is built, so those only apply to
        copies put into play after the save.
      </p>

      <CustomCardForm ref="form" />
    </main>
  </div>
</template>

<style scoped lang="scss">
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
  flex: 0 0 280px;
  background: var(--background-dark);
  border: 1px solid var(--box-border);
  border-radius: 8px;
  padding: 1rem;
  max-height: calc(100vh - var(--nav-height) - 3rem);
  overflow: auto;
  position: sticky;
  top: 1.5rem;

  @media (max-width: 900px) {
    position: static;
    flex: 1 1 auto;
    width: 100%;
    max-height: none;
  }
}

.library-head {
  align-items: center;
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.5rem;

  h2 {
    font-family: teutonic, sans-serif;
    font-size: 1.3em;
    margin: 0;
  }
}

.library-tools {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
  margin-bottom: 0.75rem;
}

.import {
  cursor: pointer;
  font-size: 0.8rem;

  input {
    display: none;
  }
}

.library-list {
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  list-style: none;
  margin: 0;
  padding: 0;

  li {
    align-items: center;
    border: 1px solid transparent;
    border-radius: 6px;
    display: flex;
    gap: 0.4rem;
    padding: 0.25rem;

    &.editing {
      border-color: var(--spooky-green);
    }
  }
}

.library-card {
  align-items: center;
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  display: flex;
  flex: 1 1 auto;
  gap: 0.5rem;
  min-width: 0;
  padding: 0;
  text-align: left;

  img {
    border-radius: 4px;
    height: 46px;
    object-fit: cover;
    width: 34px;
  }

  .name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  small {
    margin-left: auto;
    opacity: 0.6;
  }
}

.row-actions button {
  background: none;
  border: none;
  color: inherit;
  cursor: pointer;
  opacity: 0.6;
  padding: 0 0.15rem;

  &:hover {
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
