<script lang="ts" setup>
import { ref, inject } from 'vue'
import * as Arkham from '@/arkham/types/Deck'
import Prompt from '@/components/Prompt.vue'
import { fetchDecks, deleteDeck, syncDeck } from '@/arkham/api'
import NewDeck from '@/arkham/components/NewDeck';
import { useToast } from "vue-toastification";

const ready = ref(false)
const decks = ref<Arkham.Deck[]>([])
const baseUrl = inject('baseUrl')
const deleteId = ref<string | null>(null)

const toast = useToast()

async function addDeck(d: Arkham.Deck) {
  decks.value.push(d)
}

async function deleteDeckEvent() {
  const { value } = deleteId
  if (value) {
    deleteDeck(value).then(() => {
      decks.value = decks.value.filter((deck) => deck.id !== value)
      deleteId.value = null
    })
  }
}

fetchDecks().then(async (response) => {
  decks.value = response
  ready.value = true
})

async function sync(deck: Arkham.Deck) {
  syncDeck(deck.id).then(() => {
    toast.success("Deck synced successfully", { timeout: 3000 })
  })
}

const deckUrlToPage = (url: string): string => {
  // converts https://arkhamdb.com/api/public/decklist/25027
  // to https://arkhamdb.com/decklist/view/25027
  // OR
  // converts https://arkhamdb.com/api/public/deck/25027
  // to https://arkhamdb.com/deck/view/25027
  return url.replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
}
</script>

<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <NewDeck @new-deck="addDeck"/>
    </div>
    <h2>Existing Decks</h2>
    <transition-group name="deck">
      <div v-for="deck in decks" :key="deck.id" class="deck">
        <img class="portrait--decklist" :src="`${baseUrl}/img/arkham/cards/${deck.list.investigator_code.replace('c', '')}.jpg`" />
        <span class="deck-title">{{deck.name}}</span>
        <div class="open-deck">
          <a :href="deckUrlToPage(deck.url)" target="_blank" rel="noreferrer noopener"><font-awesome-icon alt="View Deck in ArkhamDB" icon="external-link" /></a>
        </div>
        <div class="sync-deck">
          <a href="#" @click.prevent="sync(deck)"><font-awesome-icon icon="refresh" /></a>
        </div>
        <div class="deck-delete">
          <a href="#delete" @click.prevent="deleteId = deck.id"><font-awesome-icon icon="trash" /></a>
        </div>
      </div>
    </transition-group>

    <Prompt
      v-if="deleteId"
      prompt="Are you sure you want to delete this deck?"
      :yes="deleteDeckEvent"
      :no="() => deleteId = null"
    />
  </div>
</template>

<style lang="scss" scoped>
.deck {
  display: flex;
  background-color: #15192C;
  color: #f0f0f0;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  span {
    flex: 1;
  }
  a {
    color: #365488;
    font-weight: bolder;
  }
}

h2 {
  color: #656A84;
  margin-left: 10px;
  text-transform: uppercase;
}

#decks {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
}

.open-deck {
  justify-self: flex-end;
  align-self: flex-start;
  margin-right: 10px;
}

.sync-deck {
  justify-self: flex-end;
  align-self: flex-start;
  margin-right: 10px;
}

.deck-delete {
  justify-self: flex-end;
  align-self: flex-start;
  a {
    color: #660000;
    &:hover {
      color: #990000;
    }
  }
}

.portrait--decklist {
  width: 100px;
  margin-right: 10px;
}

.deck-title {
  font-weight: 800;
  font-size: 1.2em;
}

.deck-move,
.deck-enter-active,
.deck-leave-active {
  transition: all 0.5s ease;
}

.deck-enter-from,
.deck-leave-to {
  opacity: 0;
  transform: translateX(30px);
}

.deck-leave-active {
  position: absolute;
}

</style>
