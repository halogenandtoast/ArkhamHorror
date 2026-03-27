<script lang="ts" setup>
import { ref, computed } from 'vue'
import * as Arkham from '@/arkham/types/Deck'
import Prompt from '@/components/Prompt.vue'
import { fetchDecks, deleteDeck, syncDeck } from '@/arkham/api'
import { capitalize } from '@/arkham/helpers'
import NewDeck from '@/arkham/components/NewDeck.vue';
import Deck from '@/arkham/components/DeckRow.vue';
import PrimaryButton from '@/components/PrimaryButton.vue';
import { useToast } from "vue-toastification";

const allDecks = ref<Arkham.Deck[]>([])
const deleteId = ref<string | null>(null)
const toast = useToast()
const showNewDeck = ref(false)
const searchText = ref('')
const sortBy = ref<'name' | 'class'>('name')

interface Filter {
  classes: string[]
}

const allClasses = ["guardian", "seeker", "rogue", "mystic", "survivor", "neutral"]

const CLASS_ORDER: Record<string, number> = {
  guardian: 0, seeker: 1, rogue: 2, mystic: 3, survivor: 4, neutral: 5
}

const filter = ref<Filter>({ classes: [] })

async function addDeck(d: Arkham.Deck) {
  allDecks.value.push(d)
  showNewDeck.value = false
}

async function deleteDeckEvent() {
  const { value } = deleteId
  if (value) {
    deleteDeck(value).then(() => {
      allDecks.value = allDecks.value.filter((deck) => deck.id !== value)
      deleteId.value = null
    })
  }
}

fetchDecks().then(async (response) => {
  allDecks.value = response
})

const decks = computed(() => {
  let result = allDecks.value.filter((deck) => {
    const matchesClass = filter.value.classes.length === 0 ||
      filter.value.classes.some((k) => Arkham.deckClass(deck)[k])
    const matchesSearch = !searchText.value ||
      deck.name.toLowerCase().includes(searchText.value.toLowerCase())
    return matchesClass && matchesSearch
  })

  if (sortBy.value === 'name') {
    result = [...result].sort((a, b) => a.name.localeCompare(b.name))
  } else if (sortBy.value === 'class') {
    result = [...result].sort((a, b) => {
      const classObj = (d: Arkham.Deck) => Arkham.deckClass(d)
      const ca = allClasses.find(k => classObj(a)[k]) ?? 'neutral'
      const cb = allClasses.find(k => classObj(b)[k]) ?? 'neutral'
      return (CLASS_ORDER[ca] ?? 5) - (CLASS_ORDER[cb] ?? 5)
    })
  }

  return result
})

async function sync(deck: Arkham.Deck) {
  syncDeck(deck.id).then(() => {
    toast.success("Deck synced successfully", { timeout: 3000 })
  })
}

function toggleClass(c: string) {
  const { classes } = filter.value
  const index = classes.indexOf(c)
  if (index === -1) {
    classes.push(c)
  } else {
    classes.splice(index, 1)
  }
}
</script>

<template>
  <div class="page-container">
    <div id="decks">
      <header class="decks-header">
        <h2>My Decks</h2>
        <PrimaryButton :label="showNewDeck ? 'Cancel' : 'New Deck'" @click="showNewDeck = !showNewDeck" />
      </header>

      <div v-if="showNewDeck" class="new-deck-panel">
        <NewDeck @new-deck="addDeck" />
      </div>

      <div class="toolbar">
        <div class="class-filters">
          <button
            v-for="iclass in allClasses"
            :key="iclass"
            class="class-pill"
            :class="{ [iclass]: filter.classes.includes(iclass), active: filter.classes.includes(iclass) }"
            @click="toggleClass(iclass)"
          >
            <span :class="`${iclass}-icon`"></span>
            {{ capitalize(iclass) }}
          </button>
        </div>
        <div class="toolbar-right">
          <input
            v-model="searchText"
            class="search-input"
            placeholder="Search decks…"
            type="search"
          />
          <select v-model="sortBy" class="sort-select">
            <option value="name">Sort: Name</option>
            <option value="class">Sort: Class</option>
          </select>
        </div>
      </div>

      <div v-if="decks.length === 0" class="empty-state">
        <p>No decks match your filters.</p>
      </div>
      <div v-else class="deck-grid">
        <Deck
          v-for="deck in decks"
          :key="deck.id"
          :deck="deck"
          :markDelete="() => deleteId = deck.id"
          :sync="() => sync(deck)"
        />
      </div>

      <Prompt
        v-if="deleteId"
        prompt="Are you sure you want to delete this deck?"
        :yes="deleteDeckEvent"
        :no="() => deleteId = null"
      />
    </div>
  </div>
</template>

<style scoped>
#decks {
  max-width: 1200px;
  margin: 0 auto;
  padding: 24px 20px;
}

.decks-header {
  display: flex;
  align-items: center;
  margin-bottom: 10px;

  h2 {
    flex: 1;
    color: var(--title);
    font-size: 2em;
    text-transform: uppercase;
    font-family: teutonic, sans-serif;
    margin: 0;
  }
}

.new-deck-panel {
  background: #111;
  border: 1px solid #2a2a2a;
  border-radius: 8px;
  padding: 20px;
  margin-bottom: 20px;
}

.toolbar {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 10px;
  margin-bottom: 20px;
}

.class-filters {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
  flex: 1;
}

.class-pill {
  display: flex;
  align-items: center;
  gap: 5px;
  padding: 5px 10px;
  font-size: 0.78rem;
  font-weight: 600;
  color: #666;
  background: #1a1a1a;
  border: 1px solid #2a2a2a;
  border-radius: 4px;
  cursor: pointer;
  transition: background 0.12s, color 0.12s, border-color 0.12s;
  user-select: none;

  &:hover {
    color: #aaa;
    border-color: #444;
  }

  &.active.guardian { background: var(--guardian-extra-dark); border-color: var(--guardian-dark); color: #fff; }
  &.active.seeker   { background: var(--seeker-extra-dark);   border-color: var(--seeker-dark);   color: #fff; }
  &.active.rogue    { background: var(--rogue-extra-dark);    border-color: var(--rogue-dark);    color: #fff; }
  &.active.mystic   { background: var(--mystic-extra-dark);   border-color: var(--mystic-dark);   color: #fff; }
  &.active.survivor { background: var(--survivor-extra-dark); border-color: var(--survivor-dark); color: #fff; }
  &.active.neutral  { background: var(--neutral-extra-dark);  border-color: var(--neutral-dark);  color: #fff; }
}

.toolbar-right {
  display: flex;
  align-items: center;
  gap: 8px;
}

.search-input {
  padding: 6px 10px;
  font-size: 0.82rem;
  color: #ccc;
  background: #1a1a1a;
  border: 1px solid #2a2a2a;
  border-radius: 4px;
  outline: none;
  width: 180px;
  transition: border-color 0.12s;

  &::placeholder { color: #444; }
  &:focus { border-color: #444; }
}

.sort-select {
  width: max-content;
  padding: 6px 32px 6px 10px;
  font-size: 0.82rem;
  color: #ccc;
  background-color: #1a1a1a;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='10' height='6'%3E%3Cpath d='M0 0l5 6 5-6z' fill='%23888'/%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 10px center;
  border: 1px solid #2a2a2a;
  border-radius: 4px;
  outline: none;
  cursor: pointer;
  appearance: none;

  option { background: #1a1a1a; }
}

.empty-state {
  padding: 40px;
  text-align: center;
  color: #555;
  font-size: 0.9rem;
}

.deck-grid {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

</style>
