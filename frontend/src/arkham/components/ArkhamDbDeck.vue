<script lang="ts" setup>
import { ref, defineModel } from 'vue'

const model = defineModel()

const deck = ref<string | null>(null)
const deckUrl = ref<string | null>(null)

function loadDeck() {
  model.value = null
  const matches = deck.value.match(/\/(deck(list)?)(\/view)?\/([^/]+)/)
  if (matches) {
    deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
    fetch(deckUrl.value)
      .then((response) => response.json(), () => model.value = null)
      .then((data) => model.value = {...data, url: deckUrl.value}, () => model.value = null)
  }
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text')
    loadDeck()
  }
}
</script>

<template>
  <input
    type="url"
    v-model="deck"
    @change="loadDeck"
    @paste.prevent="pasteDeck($event)"
    placeholder="ArkhamDB deck url"
  />
</template>

<style lang="scss" scoped>
input {
  outline: 0;
  border: 1px solid #000;
  padding: 15px;
  background: #F2F2F2;
  width: 100%;
  box-sizing: border-box;
  margin-bottom: 10px;
}
</style>
