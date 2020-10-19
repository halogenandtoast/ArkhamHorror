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
import { Component, Vue } from 'vue-property-decorator';
import * as Arkham from '@/arkham/types/Deck';
import { fetchDecks, newDeck } from '@/arkham/api';

@Component
export default class Decks extends Vue {
  private ready = false;
  private decks: Arkham.Deck[] = [];

  deck: string | null = null
  investigator: string | null = null
  deckId: string | null = null
  deckName: string | null = null

  async mounted() {
    fetchDecks().then((decks) => {
      this.decks = decks;
      this.ready = true;
    });
  }

  pasteDeck(evt: ClipboardEvent) {
    if (evt.clipboardData) {
      this.deck = evt.clipboardData.getData('text');
      this.loadDeck();
    }
  }

  loadDeck() {
    if (!this.deck) {
      return;
    }

    const matches = this.deck.match(/\/decklist(\/view)?\/([^/]+)/);
    if (matches && matches[2]) {
      fetch(`https://arkhamdb.com/api/public/decklist/${matches[2]}`)
        .then((response) => response.json())
        .then((data) => {
          const deckId = matches[2];
          this.investigator = data.investigator_code;
          this.deckId = deckId;
          this.deckName = data.name;
        });
    }
  }

  createDeck() {
    if (this.deckId && this.deckName) {
      newDeck(this.deckId, this.deckName).then((deck) => this.decks.push(deck));
      this.deckId = null;
      this.deckName = null;
      this.investigator = null;
      this.deck = null;
    }
  }
}
</script>

<style lang="scss" scoped>
</style>
