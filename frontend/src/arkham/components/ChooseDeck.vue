<script lang="ts" setup>
import { computed, ref, inject } from 'vue'
import type { Game } from '@/arkham/types/Game';
import { fetchDecks } from '@/arkham/api'
import * as Arkham from '@/arkham/types/Deck'

const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const deckId = ref<string | null>(null)

defineProps<{
  game: Game
  playerId: string
}>()

const chooseDeck = inject<(deckId: string) => Promise<void>>('chooseDeck')

const emit = defineEmits<{
  update: [game: Game]
  choose: [idx: number]
}>()

const disabled = computed(() => !deckId.value)

fetchDecks().then((result) => {
  decks.value = result;
  ready.value = true;
})

async function choose() {
  if (deckId.value) {
    await chooseDeck(deckId.value)
  }
}
</script>

<template>
  <form id="choose-deck" @submit.prevent="choose">
    <p>Deck</p>
    <select v-model="deckId">
      <option disabled :value="null">-- Select a Deck--</option>
      <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
    </select>
    <button type="submit" :disabled="disabled">Choose</button>
  </form>
</template>
