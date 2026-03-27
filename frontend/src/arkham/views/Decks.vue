<script lang="ts" setup>
import { ref, computed } from 'vue'
import * as Arkham from '@/arkham/types/Deck'
import Prompt from '@/components/Prompt.vue'
import { fetchDecks, deleteDeck, syncDeck } from '@/arkham/api'
import NewDeck from '@/arkham/components/NewDeck.vue';
import Deck from '@/arkham/components/DeckRow.vue';
import DeckToolbar from '@/arkham/components/DeckToolbar.vue';
import PrimaryButton from '@/components/PrimaryButton.vue';
import { useToast } from "vue-toastification";

const allDecks = ref<Arkham.Deck[]>([])
const deleteId = ref<string | null>(null)
const toast = useToast()
const showNewDeck = ref(false)
const searchText = ref('')
const sortBy = ref<'name' | 'class'>('name')
const filterClasses = ref<string[]>([])

const CLASS_ORDER: Record<string, number> = {
  guardian: 0, seeker: 1, rogue: 2, mystic: 3, survivor: 4, neutral: 5
}
const allClasses = ["guardian", "seeker", "rogue", "mystic", "survivor", "neutral"]

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
    const matchesClass = filterClasses.value.length === 0 ||
      filterClasses.value.some((k) => Arkham.deckClass(deck)[k])
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
</script>

<template>
  <div class="page-container">
    <div id="decks">
      <header class="decks-header">
        <h2>My Decks</h2>
        <PrimaryButton :label="showNewDeck ? 'Cancel' : 'New Deck'" :danger="showNewDeck" @click="showNewDeck = !showNewDeck" />
      </header>

      <div v-if="showNewDeck" class="new-deck-panel">
        <NewDeck always-save @new-deck="addDeck" />
      </div>

      <DeckToolbar
        v-model:search="searchText"
        v-model:filterClasses="filterClasses"
        v-model:sortBy="sortBy"
        class="toolbar"
      />

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
  width: 70vw;
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
  padding: 0 20px;
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
  margin-bottom: 20px;
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
