<script lang="ts" setup>
import { ref, inject } from 'vue'
import * as Arkham from '@/arkham/types/Deck'
import Prompt from '@/components/Prompt.vue'
import { fetchInvestigators, fetchDecks, newDeck, deleteDeck, syncDeck } from '@/arkham/api'
import * as Investigator from '@/arkham/types/Investigator';
import { useToast } from "vue-toastification";

interface UnimplementedCardError {
  tag: string
  contents: string
}

interface ArkhamDBCard {
  name: string
  code: string
  xp?: string
}

const ready = ref(false)
const decks = ref<Arkham.Deck[]>([])
const baseUrl = inject('baseUrl')
const errors = ref([])
const investigatorError = ref<string | null>(null)

const deck = ref<string | null>(null)
const investigators = ref<Investigator.Investigator[]>([])
const investigator = ref<string | null>(null)
const deckId = ref<string | null>(null)
const deckName = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const deleteId = ref<string | null>(null)

const toast = useToast()

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
  await fetchInvestigators().then((response) => investigators.value = response)
  ready.value = true
})

function loadDeck() {
  if (!deck.value) {
    return
  }

  const matches = deck.value.match(/\/(deck(list)?)(\/view)?\/([^/]+)/)
  if (matches) {
    deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
    fetch(deckUrl.value)
      .then((response) => response.json(), () => {
        investigator.value = null
        deckId.value = null
        deckName.value = null
        deckUrl.value = null
      })
      .then((data) => {
        investigator.value = null
        investigatorError.value = null
        if (investigators.value.map(i => i.id.replace(/^c/, '')).includes(data.investigator_code)) {
          if(data.meta && data.meta.alternate_front) {
            investigator.value = data.meta.alternate_front
          } else {
            investigator.value = data.investigator_code
          }
        } else {
          investigatorError.value = `${data.investigator_name} is not yet implemented, please use a different deck`
        }
        deckId.value = matches[4]
        deckName.value = data.name
      })
  } else {
    investigator.value = null
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
  }
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text')
    loadDeck()
  }
}

const deckUrlToPage = (url: string): string => {
  // converts https://arkhamdb.com/api/public/decklist/25027
  // to https://arkhamdb.com/decklist/view/25027
  // OR
  // converts https://arkhamdb.com/api/public/deck/25027
  // to https://arkhamdb.com/deck/view/25027
  return url.replace("/api/public/decklist", "/decklist/view").replace("/api/public/deck", "/deck/view")
}

async function sync(deck: Arkham.Deck) {
  syncDeck(deck.id).then(() => {
    toast.success("Deck synced successfully", { timeout: 3000 })
  })
}

async function createDeck() {
  errors.value = []
  if (deckId.value && deckName.value && deckUrl.value) {
    newDeck(deckId.value, deckName.value, deckUrl.value).then((newDeck) => {
      decks.value.push(newDeck)
      deckId.value = null
      deckName.value = null
      deckUrl.value = null
      investigator.value = null
      deck.value = null
    }).catch((error) => {
      fetch("https://arkhamdb.com/api/public/cards/")
        .then((response) => response.json()).then((data) => {
          errors.value = error.response.data.map((error: UnimplementedCardError) => {
            const match = data.find((c: ArkhamDBCard) => c.code == error.contents.replace(/^c/, ''))
            if (match) {
              const { name, xp } = match
              return xp ? `${name} (${xp})` : name
            }
            return "Unknown card"
          })
        })
    })
  }
}
</script>

<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <div class="new-deck">
        <div class="form-body">
          <img v-if="investigator" class="portrait" :src="`${baseUrl}/img/arkham/portraits/${investigator.replace('c', '')}.jpg`" />
          <div class="fields">
            <input
              type="url"
              v-model="deck"
              @change="loadDeck"
              @paste.prevent="pasteDeck($event)"
              placeholder="ArkhamDB deck url"
            />
            <input v-if="investigator" v-model="deckName" />
            <button :disabled="!investigator" @click.prevent="createDeck">Create</button>
          </div>
        </div>
        <div class="errors" v-if="investigatorError">
          {{investigatorError}}
        </div>
        <div class="errors" v-if="errors.length > 0">
          <p>Could not create deck, the following cards are unimplemented:</p>
          <ul>
            <li class="error" v-for="(error, idx) in errors" :key="idx">
              {{error}}
            </li>
          </ul>
        </div>
      </div>
    </div>
    <h2>Existing Decks</h2>
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

    <Prompt
      v-if="deleteId"
      prompt="Are you sure you want to delete this deck?"
      :yes="deleteDeckEvent"
      :no="() => deleteId = null"
    />
  </div>
</template>

<style lang="scss" scoped>
.new-deck {
  input {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
  }
  .portrait {
    margin-right: 10px;
    height: 170px;
  }
  .fields {
    flex: 1;
    display: flex;
    flex-direction: column;
    flex-flow: wrap;
  }
  .errors {
    background-color: #660000;
    width: 100%;
    margin-top: 10px;
    padding: 15px;
    box-sizing: border-box;
  }
  button {
    outline: 0;
    padding: 15px;
    background: #6E8640;
    text-transform: uppercase;
    color: white;
    border: 0;
    width: 100%;
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
  button[disabled] {
    background: #999;
    cursor: not-allowed;
    &:hover {
      background: #999;
    }
  }
  display: flex;
  flex-direction: column;
  color: #FFF;
  background-color: #15192C;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  a {
    color: #365488;
    font-weight: bolder;
  }
}

.form-body {
  display: flex;
}

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

</style>
