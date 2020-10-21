<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <div class="new-deck">
        <img v-if="investigator" class="portrait" :src="`/img/arkham/portraits/${investigator}.jpg`" />
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
    </div>
    <h2>Existing Decks</h2>
    <div v-for="deck in decks" :key="deck.id" class="deck">
      {{deck.name}}
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newDeck } from '@/arkham/api';

export default defineComponent({
  setup() {
    const ready = ref(false)
    const decks = ref<Arkham.Deck[]>([])

    const deck = ref<string | null>(null)
    const investigator = ref<string | null>(null)
    const deckId = ref<string | null>(null)
    const deckName = ref<string | null>(null)

    fetchDecks().then((response) => {
      decks.value = response
      ready.value = true
    })

    function loadDeck() {
      if (!deck.value) {
        return;
      }

      const matches = deck.value.match(/\/decklist(\/view)?\/([^/]+)/);
      if (matches && matches[2]) {
        fetch(`https://arkhamdb.com/api/public/decklist/${matches[2]}`)
          .then((response) => response.json())
          .then((data) => {
            investigator.value = data.investigator_code
            deckId.value = matches[2]
            deckName.value = data.name
          })
      }
    }

    function pasteDeck(evt: ClipboardEvent) {
      if (evt.clipboardData) {
        deck.value = evt.clipboardData.getData('text');
        loadDeck();
      }
    }

    async function createDeck() {
      if (deckId.value && deckName.value) {
        newDeck(deckId.value, deckName.value).then((deck) => decks.value.push(deck));
        deckId.value = null;
        deckName.value = null;
        investigator.value = null;
        deck.value = null;
      }
    }

    return { pasteDeck, createDeck, deck, decks, loadDeck, investigator, deckName }
  }
})
</script>

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

.deck {
  display: flex;
  background-color: #15192C;
  color: #f0f0f0;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
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
</style>
