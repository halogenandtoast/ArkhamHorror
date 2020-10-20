<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <p>ArkhamDB deck url</p>
      <input
        type="url"
        v-model="deck"
        @change="loadDeck"
        @paste.prevent="pasteDeck($event)"
      />
      <img v-if="investigator" :src="`/img/arkham/portraits/${investigator}.jpg`" />
      <input v-if="investigator" v-model="deckName" />
      <button :disabled="!investigator" @click.prevent="createDeck">Create</button>
    </div>
    <h2>Existing Decks</h2>
    <div v-for="deck in decks" :key="deck.id">
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

    return { pasteDeck, createDeck, deck, loadDeck, investigator, deckName }
  }
})
</script>

<style lang="scss" scoped>
</style>
