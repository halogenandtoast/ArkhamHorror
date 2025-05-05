<script lang="ts" setup>
import { ref, computed } from 'vue'
import {imgsrc} from '@/arkham/helpers';

const model = defineModel()
const deck = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const error = ref<string | null>(null)

//const isArkhamDB = computed(() => {
//  return deck.value && deck.value.match(/https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/)
//})

const isArkhamBuild = computed(() => {
  return deck.value && deck.value.match(/https:\/\/arkham\.build\/(?:deck\/view|share)\/([^/]+)/)
})

function loadDeck() {
  if (!deck.value) return
  model.value = null

  const arkhamDbRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/
  const arkhamBuildRegex = /https:\/\/arkham\.build\/((deck(list)?\/view)|share)\/([^/]+)/
  
  let matches
  if ((matches = deck.value.match(arkhamDbRegex))) {
    deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
    fetch(deckUrl.value)
      .then((response) => response.json(), () => model.value = null)
      .then((data) => model.value = {...data, url: deckUrl.value}, () => model.value = null)
  } else if ((matches = deck.value.match(arkhamBuildRegex))) {
    if (/^[0-9]+$/.test(matches[4])) {
      deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
      fetch(deckUrl.value)
        .then((response) => response.json(), () => model.value = null)
        .then((data) => model.value = {...data, url: deckUrl.value}, () => model.value = null)
    } else {
      deckUrl.value = `https://api.arkham.build/v1/public/share/${matches[4]}`
      fetch(deckUrl.value)
        .then(async (response) => {
          if (response.ok) {
            const data = await response.json()
            model.value = {...data, url: deckUrl.value}
          } else {
            model.value = null
            error.value = "Could not find deck, please make sure you have created a public share."
          }
        }, () => model.value = null)
    }
  } else {
    return
  }

}

function pasteDeck(evt: ClipboardEvent) {
  error.value = null
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
    v-bind:placeholder="$t('create.deckUrlPlaceholder')"
  />
  <div class="error" v-if="error && isArkhamBuild">
    <p>{{ error }}</p>
     <img :src="imgsrc('ui/arkham-build-public-share.jpg')" />
  </div>
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

.error {
  background-color: var(--survivor-extra-dark);
  width: 100%;
  margin-bottom: 10px;
  padding: 10px;
  border-radius: 5px;
  display: flex;
  flex-direction: column;
  gap: 10px;
  text-transform: uppercase;
  img {
    flex: 0;
    max-width: fit-content;
    border-radius: 5px;
  }
}
</style>
