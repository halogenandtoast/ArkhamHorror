<script lang="ts" setup>
import { ref } from 'vue'
import * as Arkham from '@/arkham/types/Deck'
import Prompt from '@/components/Prompt.vue'
import { fetchDecks, deleteDeck, syncDeck } from '@/arkham/api'
import NewDeck from '@/arkham/components/NewDeck.vue';
import Deck from '@/arkham/components/Deck.vue';
import { useToast } from "vue-toastification";

const ready = ref(false)
const decks = ref<Arkham.Deck[]>([])
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
        <Deck :deck="deck" :markDelete="() => deleteId = deck.id" :sync="() => sync(deck)" />
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
  a {
    text-decoration: none;
    &:hover {
      color: #336699;
    }
  }
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

.deck span.taboo-list {
  font-size: 0.8em;
  background: rgba(255, 255, 255, 0.2);
  color: #efefef;
  display: inline-block;
  width: fit-content;
  height: fit-content;
  padding: 5px;
  border-radius: 5px;
  flex: 0;
  flex-basis: fit-content;
}

.deck-details {
  display: flex;
  flex-direction: column;
  flex: 1;
}
</style>
