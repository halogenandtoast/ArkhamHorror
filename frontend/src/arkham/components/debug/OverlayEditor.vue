<script lang="ts" setup>
/* Laying your own cards over a deck: swap the investigator, swap a card for one
 * of yours, add cards, take cards out.
 *
 * Shared by the deck page, where an overlay sticks to the deck, and by deck
 * choice, where it applies to that game only. */
import { computed, onMounted } from 'vue'
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

const nameOf = (cardCode: string) => {
  const custom = library.value.find((c) => c.def.cardCode === cardCode)
  if (custom) return custom.def.name.title
  const known = cardStore.cards.find((c) => c.cardCode === cardCode)
  return known ? known.name.title : cardCode
}

const deckCards = computed(() =>
  Object.entries(props.slots)
    .map(([cardCode, count]) => ({ cardCode, count, name: nameOf(cardCode) }))
    .sort((a, b) => a.name.localeCompare(b.name)),
)

/* Slot keys arrive both bare and with the 'c' the engine prepends. */
const slotKey = (code: string) => {
  const bare = code.replace(/^c/, '')
  if (props.slots[bare] !== undefined) return bare
  if (props.slots[`c${bare}`] !== undefined) return `c${bare}`
  return null
}

/* The engine marks signature assets and events but never signature weaknesses,
 * so replacing the investigator would leave Tommy Malloy and his kind behind.
 * ArkhamDB records what an investigator's deck requires, which is that list. */
const oldSignatureKeys = computed(() => {
  const required = dbStore.getDbCard(props.investigator.replace(/^c/, ''))?.deck_requirements?.card ?? {}
  const codes = Object.entries(required).flatMap(([code, alternates]) => [
    code,
    ...Object.keys(alternates ?? {}),
  ])
  return [...new Set(codes.map(slotKey).filter((key): key is string => key !== null))]
})

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

const removedCount = (cardCode: string) => overlay.value.remove[cardCode] ?? 0

function setRemoved(cardCode: string, n: number) {
  const remove = { ...overlay.value.remove }
  if (n <= 0) delete remove[cardCode]
  else remove[cardCode] = n
  patch({ remove })
}

function setSwap(cardCode: string, to: string) {
  const swaps = { ...overlay.value.swaps }
  if (!to) delete swaps[cardCode]
  else swaps[cardCode] = to
  patch({ swaps })
}

const addedCount = (cardCode: string) => overlay.value.add[cardCode] ?? 0

function setAdded(cardCode: string, n: number) {
  const add = { ...overlay.value.add }
  if (n <= 0) delete add[cardCode]
  else add[cardCode] = n
  patch({ add })
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

defineExpose({ isEmpty })
</script>

<template>
  <div class="overlay-editor">
    <p v-if="!libraryLoaded" class="muted">Loading your library…</p>

    <template v-else-if="!library.length">
      <p class="muted">Your card library is empty. Build a card first and it will be offered here.</p>
    </template>

    <template v-else>
      <label>
        Investigator
        <select
          :value="overlay.investigator ?? ''"
          @change="setInvestigator(($event.target as HTMLSelectElement).value || null)"
        >
          <option value="">Unchanged</option>
          <option v-for="card in investigators" :key="card.def.cardCode" :value="card.def.cardCode">
            {{ card.def.name.title }}
          </option>
        </select>
      </label>
      <p v-if="overlay.investigator" class="hint">
        Their signature cards replace the ones the deck's investigator brought.
      </p>

      <details open>
        <summary>Cards in this deck</summary>
        <ul class="deck-cards">
          <li v-for="card in deckCards" :key="card.cardCode">
            <span class="card-name">{{ card.count }}× {{ card.name }}</span>
            <label class="inline">
              remove
              <input
                type="number"
                min="0"
                :max="card.count"
                :value="removedCount(card.cardCode)"
                @input="setRemoved(card.cardCode, Number(($event.target as HTMLInputElement).value))"
                @keydown.stop
              />
            </label>
            <select :value="overlay.swaps[card.cardCode] ?? ''" @change="setSwap(card.cardCode, ($event.target as HTMLSelectElement).value)">
              <option value="">no swap</option>
              <option v-for="c in playerCards" :key="c.def.cardCode" :value="c.def.cardCode">
                → {{ c.def.name.title }}
              </option>
            </select>
          </li>
        </ul>
      </details>

      <details>
        <summary>Add from your library</summary>
        <ul class="deck-cards">
          <li v-for="card in playerCards" :key="card.def.cardCode">
            <span class="card-name">{{ card.def.name.title }}</span>
            <label class="inline">
              add
              <input
                type="number"
                min="0"
                :value="addedCount(card.def.cardCode)"
                @input="setAdded(card.def.cardCode, Number(($event.target as HTMLInputElement).value))"
                @keydown.stop
              />
            </label>
          </li>
        </ul>
      </details>

      <button v-if="!isEmpty" type="button" class="link" @click="emit('update:modelValue', null)">
        Clear overlay
      </button>
    </template>
  </div>
</template>

<style scoped lang="scss">
.overlay-editor {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  font-size: 0.85rem;
}

label {
  display: flex;
  flex-direction: column;
  gap: 0.2rem;

  &.inline {
    align-items: center;
    flex-direction: row;
    gap: 0.3rem;
  }
}

select,
input {
  background: var(--background-dark, #111827);
  border: 1px solid var(--box-border, #4b5563);
  border-radius: 4px;
  color: inherit;
  padding: 0.25rem;
}

.inline input {
  width: 3.5rem;
}

.deck-cards {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  list-style: none;
  margin: 0.4rem 0 0;
  max-height: 40vh;
  overflow: auto;
  padding: 0;

  li {
    align-items: center;
    display: flex;
    gap: 0.4rem;
  }
}

.card-name {
  flex: 1 1 auto;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

summary {
  cursor: pointer;
  opacity: 0.85;
}

.hint,
.muted {
  margin: 0;
  opacity: 0.75;
}

.link {
  align-self: flex-start;
  background: none;
  border: none;
  color: var(--spooky-green, #adf);
  cursor: pointer;
  padding: 0;
}
</style>
