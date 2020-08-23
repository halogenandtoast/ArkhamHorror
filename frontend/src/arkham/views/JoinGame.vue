<template>
  <form id="join-game" @submit.prevent="join">
    <div>
      <p>ArkhamDB deck url</p>
      <input
        type="url"
        v-model="deck"
        @change="loadDeck"
        @paste.prevent="pasteDeck($event)"
      />
      <img v-if="investigator" :src="`/img/arkham/portraits/${investigator}.jpg`" />
    </div>

    <button type="submit" :disabled="disabled">Join</button>
  </form>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { joinGame } from '@/arkham/api';

@Component
export default class NewCampaign extends Vue {
  @Prop(String) readonly gameId!: string;

  deck: string | null = null
  investigator: string | null = null
  deckId: string | null = null

  get disabled() {
    return !this.investigator;
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
        });
    }
  }

  join() {
    if (this.deckId) {
      joinGame(this.gameId, this.deckId)
        .then((game) => this.$router.push(`/games/${game.id}`));
    }
  }
}
</script>
