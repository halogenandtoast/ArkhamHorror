<template>
  <div v-if="ready">
    <div v-if="decks.length == 0">
      No decks, please add one first here <router-link to="/decks">here</router-link>
    </div>
    <form v-else id="join-game" @submit.prevent="join">
      <div>
        <p>Deck</p>
        <select v-model="deckId">
          <option disabled :value="null">-- Select a Deck--</option>
          <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
        </select>
      </div>

      <button type="submit" :disabled="disabled">Join</button>
    </form>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from 'vue-property-decorator';
import { fetchDecks, joinGame } from '@/arkham/api';
import * as Arkham from '@/arkham/types/Deck';

@Component
export default class NewCampaign extends Vue {
  @Prop(String) readonly gameId!: string;
  private decks: Arkham.Deck[] = [];

  deckId: string | null = null
  ready = false

  async mounted() {
    fetchDecks().then((decks) => {
      this.decks = decks;
      this.ready = true;
    });
  }

  get disabled() {
    return !this.deckId;
  }

  join() {
    if (this.deckId) {
      joinGame(this.gameId, this.deckId)
        .then((game) => this.$router.push(`/games/${game.id}`));
    }
  }
}
</script>
