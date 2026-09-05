<script lang="ts" setup>
/* Laying custom cards over a deck: put a custom investigator in front of it,
 * add cards from your library, take cards out.
 *
 * Adding is a searchable list rather than a dropdown: a library grows without
 * bound, and a select stops being usable long before it stops fitting.
 *
 * Shared by the deck page (where the overlay sticks to the deck), deck choice
 * (that game only), and the campaign continuation screen (the rest of the
 * campaign). */
import { computed, onMounted, ref } from 'vue'
import { useCardStore } from '@/stores/cards'
import { useDbCardStore } from '@/stores/dbCards'
import { libraryCards, libraryLoaded, loadLibrary } from '@/arkham/customCardLibrary'

export type DeckOverlay = {
  investigator: string | null
  swaps: Record<string, string>
  add: Record<string, number>
  remove: Record<string, number>
}

const props = defineProps<{
  slots: Record<string, number>
  /** The deck's own investigator, whose signatures a replacement displaces. */
  investigator: string
  modelValue: DeckOverlay | null
}>()
const emit = defineEmits<{ 'update:modelValue': [v: DeckOverlay | null] }>()

const cardStore = useCardStore()
const dbStore = useDbCardStore()

onMounted(() => {
  loadLibrary()
  cardStore.fetchCards()
  dbStore.initDbCards()
})

const empty = (): DeckOverlay => ({ investigator: null, swaps: {}, add: {}, remove: {} })
const overlay = computed(() => props.modelValue ?? empty())

const patch = (changes: Partial<DeckOverlay>) => emit('update:modelValue', { ...overlay.value, ...changes })

const library = computed(() => libraryCards())
const investigators = computed(() => library.value.filter((c) => c.def.cardType === 'InvestigatorType'))
const playerCards = computed(() => library.value.filter((c) => c.def.cardType !== 'InvestigatorType'))

const addSearch = ref('')
const deckSearch = ref('')

const matches = (text: string, term: string) => text.toLowerCase().includes(term.trim().toLowerCase())

const addable = computed(() =>
  playerCards.value.filter((c) => !addSearch.value || matches(c.def.name.title, addSearch.value)),
)

/* Grouped by the set they name, the way the card builder's library groups them,
 * so a set built together stays together here too. */
const addableGroups = computed(() => {
  const groups = new Map<string, typeof addable.value>()
  for (const card of addable.value) {
    const set = (card.def.meta?.set as string)?.trim() || 'Ungrouped'
    if (!groups.has(set)) groups.set(set, [])
    groups.get(set)!.push(card)
  }
  for (const cards of groups.values()) {
    cards.sort((a, b) =>
      ((a.def.meta?.number as string) ?? '').localeCompare(
        (b.def.meta?.number as string) ?? '',
        undefined,
        { numeric: true },
      ) || a.def.name.title.localeCompare(b.def.name.title),
    )
  }
  return [...groups.entries()].sort(([a], [b]) =>
    a === 'Ungrouped' ? 1 : b === 'Ungrouped' ? -1 : a.localeCompare(b),
  )
})

const nameOf = (cardCode: string) => {
  const custom = library.value.find((c) => c.def.cardCode === cardCode)
  if (custom) return custom.def.name.title
  const known = cardStore.cards.find((c) => c.cardCode === cardCode)
  return known ? known.name.title : cardCode
}

const deckCards = computed(() =>
  Object.entries(props.slots)
    .map(([cardCode, count]) => ({ cardCode, count, name: nameOf(cardCode) }))
    .filter((c) => !deckSearch.value || matches(c.name, deckSearch.value))
    .sort((a, b) => a.name.localeCompare(b.name)),
)

// --- investigator ---

/* The engine marks signature assets and events but never signature weaknesses,
 * so a replaced investigator would leave theirs behind. ArkhamDB records what an
 * investigator's deck requires, which is that list. */
const oldSignatureKeys = computed(() => {
  const required = dbStore.getDbCard(props.investigator.replace(/^c/, ''))?.deck_requirements?.card ?? {}
  const codes = Object.entries(required).flatMap(([code, alternates]) => [
    code,
    ...Object.keys(alternates ?? {}),
  ])
  return [...new Set(codes.map(slotKey).filter((key): key is string => key !== null))]
})

/* Slot keys arrive both bare and with the 'c' the engine prepends. */
function slotKey(code: string) {
  const bare = code.replace(/^c/, '')
  if (props.slots[bare] !== undefined) return bare
  if (props.slots[`c${bare}`] !== undefined) return `c${bare}`
  return null
}

/* Taking their investigator out takes their signatures with it; putting them
 * back restores whatever the deck had. Both stay visible and editable below. */
function setInvestigator(code: string | null) {
  const remove = { ...overlay.value.remove }
  for (const key of oldSignatureKeys.value) {
    if (code) remove[key] = props.slots[key]
    else if (remove[key] === props.slots[key]) delete remove[key]
  }
  patch({ investigator: code, remove })
}

// --- adding and removing ---

const addedCount = (cardCode: string) => overlay.value.add[cardCode] ?? 0

function setAdded(cardCode: string, n: number) {
  const add = { ...overlay.value.add }
  if (n <= 0) delete add[cardCode]
  else add[cardCode] = n
  patch({ add })
}

const removedCount = (cardCode: string) => overlay.value.remove[cardCode] ?? 0

function setRemoved(cardCode: string, n: number, max: number) {
  const remove = { ...overlay.value.remove }
  const clamped = Math.max(0, Math.min(n, max))
  if (clamped === 0) delete remove[cardCode]
  else remove[cardCode] = clamped
  patch({ remove })
}

const isEmpty = computed(() => {
  const o = overlay.value
  return (
    !o.investigator &&
    Object.keys(o.swaps).length === 0 &&
    Object.keys(o.add).length === 0 &&
    Object.keys(o.remove).length === 0
  )
})

const addedTotal = computed(() => Object.values(overlay.value.add).reduce((a, b) => a + b, 0))
const removedTotal = computed(() => Object.values(overlay.value.remove).reduce((a, b) => a + b, 0))

defineExpose({ isEmpty })
</script>

<template>
  <div class="overlay-editor">
    <p v-if="!libraryLoaded" class="muted">Loading your library…</p>

    <p v-else-if="!library.length" class="muted">
      Your custom card library is empty. Build a card first and it will be offered here.
    </p>

    <template v-else>
      <label v-if="investigators.length" class="field">
        <span class="field-label">Investigator</span>
        <select
          :value="overlay.investigator ?? ''"
          @change="setInvestigator(($event.target as HTMLSelectElement).value || null)"
        >
          <option value="">Keep the deck's own</option>
          <option v-for="card in investigators" :key="card.def.cardCode" :value="card.def.cardCode">
            {{ card.def.name.title }}
          </option>
        </select>
      </label>
      <p v-if="overlay.investigator" class="hint">
        Their signature cards replace the ones the deck's investigator brought.
      </p>

      <section v-if="playerCards.length">
        <header class="section-head">
          <h4>Add cards<span v-if="addedTotal" class="count">+{{ addedTotal }}</span></h4>
          <input v-model="addSearch" class="search" type="search" placeholder="Search…" @keydown.stop />
        </header>
        <ul class="rows">
          <template v-for="[set, cards] in addableGroups" :key="set">
            <li v-if="addableGroups.length > 1 || set !== 'Ungrouped'" class="group">{{ set }}</li>
            <li
              v-for="card in cards"
              :key="card.def.cardCode"
              :class="{ picked: addedCount(card.def.cardCode) > 0 }"
            >
              <span class="row-name">{{ card.def.name.title }}</span>
              <span class="tally">{{ addedCount(card.def.cardCode) || '' }}</span>
              <span class="stepper">
                <button
                  type="button"
                  :disabled="addedCount(card.def.cardCode) === 0"
                  title="Add one fewer"
                  @click="setAdded(card.def.cardCode, addedCount(card.def.cardCode) - 1)"
                >−</button>
                <button
                  type="button"
                  title="Add one"
                  @click="setAdded(card.def.cardCode, addedCount(card.def.cardCode) + 1)"
                >+</button>
              </span>
            </li>
          </template>
        </ul>
        <p v-if="!addable.length" class="muted">No card matches that.</p>
      </section>

      <section v-if="Object.keys(props.slots).length">
        <header class="section-head">
          <h4>Take cards out<span v-if="removedTotal" class="count">−{{ removedTotal }}</span></h4>
          <input v-model="deckSearch" class="search" type="search" placeholder="Search…" @keydown.stop />
        </header>
        <ul class="rows">
          <li
            v-for="card in deckCards"
            :key="card.cardCode"
            :class="{ out: removedCount(card.cardCode) >= card.count }"
          >
            <span class="row-name">{{ card.name }}</span>
            <span class="tally">{{ card.count - removedCount(card.cardCode) }} / {{ card.count }}</span>
            <span class="stepper">
              <button
                type="button"
                :disabled="removedCount(card.cardCode) >= card.count"
                title="Take one out"
                @click="setRemoved(card.cardCode, removedCount(card.cardCode) + 1, card.count)"
              >−</button>
              <button
                type="button"
                :disabled="removedCount(card.cardCode) === 0"
                title="Put one back"
                @click="setRemoved(card.cardCode, removedCount(card.cardCode) - 1, card.count)"
              >+</button>
            </span>
          </li>
        </ul>
        <p v-if="!deckCards.length" class="muted">No card matches that.</p>
      </section>

      <button v-if="!isEmpty" type="button" class="clear" @click="emit('update:modelValue', null)">
        Clear overlay
      </button>
    </template>
  </div>
</template>

<style scoped lang="scss">
/* Width is the container's call: narrow inside the deck page's wide bar, full
 * width under an investigator row that is already the right size. */
.overlay-editor {
  display: flex;
  flex-direction: column;
  font-size: 0.85rem;
  gap: 0.75rem;
}

.field {
  display: flex;
  align-items: center;
  gap: 0.5rem;

  select {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid rgba(255, 255, 255, 0.18);
    border-radius: 4px;
    color: inherit;
    font-size: 0.85rem;
    padding: 0.25rem 0.4rem;
  }
}

.field-label,
.section-head h4 {
  font-size: 0.72rem;
  letter-spacing: 0.06em;
  margin: 0;
  opacity: 0.75;
  text-transform: uppercase;
  white-space: nowrap;
}

.section-head {
  align-items: center;
  border-bottom: 1px solid rgba(255, 255, 255, 0.12);
  display: flex;
  gap: 0.6rem;
  margin-bottom: 0.35rem;
  padding-bottom: 0.25rem;
}

.count {
  background: rgba(255, 255, 255, 0.12);
  border-radius: 999px;
  font-size: 0.9em;
  margin-left: 0.4em;
  padding: 0.05em 0.45em;
}

.search {
  background: rgba(0, 0, 0, 0.3);
  border: 1px solid rgba(255, 255, 255, 0.18);
  border-radius: 4px;
  color: inherit;
  font-size: 0.8rem;
  margin-left: auto;
  max-width: 12rem;
  padding: 0.2rem 0.45rem;
}

/* One row shape for both lists: a look at the card, its name, the count, and
 * the two buttons that change it. */
.rows {
  display: flex;
  flex-direction: column;
  list-style: none;
  margin: 0;
  max-height: 16rem;
  overflow-y: auto;
  padding: 0;

  li:not(.group) {
    align-items: center;
    border-radius: 4px;
    display: grid;
    gap: 0.5rem;
    grid-template-columns: minmax(0, 1fr) auto auto;
    padding: 0.15rem 0.25rem;

    &:hover {
      background: rgba(255, 255, 255, 0.05);
    }

    &.picked {
      background: color-mix(in srgb, var(--spooky-green) 12%, transparent);
    }

    /* Entirely out, but still listed so it can be put back. */
    &.out {
      opacity: 0.45;
    }
  }
}

/* A set heading inside the list rather than a nested list, so the rows keep
 * their single grid and stay aligned across groups. */
.group {
  font-size: 0.7rem;
  letter-spacing: 0.06em;
  margin-top: 0.4rem;
  opacity: 0.5;
  padding: 0.1rem 0.25rem;
  text-transform: uppercase;

  &:first-child {
    margin-top: 0;
  }
}

.row-name {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.tally {
  font-size: 0.75em;
  min-width: 2.5em;
  opacity: 0.65;
  text-align: right;
}

.stepper {
  display: flex;
  gap: 0.15rem;

  button {
    background: rgba(255, 255, 255, 0.08);
    border: 1px solid rgba(255, 255, 255, 0.18);
    border-radius: 3px;
    color: inherit;
    cursor: pointer;
    line-height: 1;
    padding: 0.1rem 0.4rem;

    &:hover:not(:disabled) {
      background: rgba(255, 255, 255, 0.18);
    }

    &:disabled {
      cursor: default;
      opacity: 0.3;
    }
  }
}

.hint,
.muted {
  font-size: 0.8em;
  margin: 0;
  opacity: 0.7;
}

.clear {
  align-self: flex-start;
  background: rgba(255, 255, 255, 0.08);
  border: 1px solid rgba(255, 255, 255, 0.22);
  border-radius: 4px;
  color: inherit;
  cursor: pointer;
  font-size: 0.8rem;
  padding: 0.3rem 0.7rem;

  &:hover {
    background: rgba(255, 255, 255, 0.16);
  }
}
</style>
