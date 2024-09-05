<script lang="ts" setup>
import { ref } from 'vue'

const model = defineModel()
const deck = ref<string | null>(null)
const deckUrl = ref<string | null>(null)

function loadDeck() {
  model.value = null
  const arkhamDbRegex = /https:\/\/arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/
  const arkhamBuildRegex = /https:\/\/arkham\.build\/deck\/view\/([^/]+)/
  
  if (deck.value?.match(arkhamDbRegex)) {
    const matches = deck.value.match(arkhamDbRegex)
    if (matches) {
      deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
      fetch(deckUrl.value)
        .then((response) => response.json(), () => model.value = null)
        .then((data) => model.value = {...data, url: deckUrl.value}, () => model.value = null)
    }
  } else if (deck.value?.match(arkhamBuildRegex)) {
    const matches = deck.value.match(arkhamBuildRegex)
    if (matches) {
      deckUrl.value = `https://api.arkham.build/v1/public/share/${matches[1]}`
      fetch(deckUrl.value)
        .then((response) => response.json(), () => model.value = null)
        .then((data) => model.value = {...data, url: deckUrl.value}, () => model.value = null)
    }
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
    placeholder="ArkhamDB or arkham.build deck url"
  />
</template>

<style lang="scss" scoped>
input {
  outline: 0;
  border: 1px solid #000;
  padding: 15px;
  background: #F2F2F2;
  width: 100%;
  margin-bottom: 10px;
}
</style>
