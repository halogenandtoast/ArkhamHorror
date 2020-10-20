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
import { defineComponent, ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { fetchDecks, joinGame } from '@/arkham/api'
import * as Arkham from '@/arkham/types/Deck'

export default defineComponent({
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const router = useRouter()
    const decks = ref<Arkham.Deck[]>([])

    const deckId = ref<string | null>(null)
    const ready = ref(false)

    fetchDecks().then((result) => {
      decks.value = result
      ready.value = true
    })

    const disabled = computed(() => !deckId.value)

    async function join() {
      if (deckId.value) {
        joinGame(props.gameId, deckId.value)
          .then((game) => router.push(`/games/${game.id}`));
      }
    }

    return { ready, deckId, decks, join, disabled }
  }
})
</script>
