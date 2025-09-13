<script lang="ts" setup>
import { ref, computed } from 'vue'
import {imgsrc, localizeArkhamDBBaseUrl} from '@/arkham/helpers';

const model = defineModel()
const deck = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const error = ref<string | null>(null)

const arkhamDbRegex = /https:\/\/(?:[a-zA-Z0-9-]+\.)?arkhamdb\.com\/(deck(list)?)(\/view)?\/([^/]+)/
const arkhamBuildRegex = /https:\/\/arkham\.build\/(deck(list)?|share)(\/view)?\/([^/]+)/
const isArkhamBuild = computed(() => deck.value && deck.value.match(arkhamBuildRegex))

async function loadDeck() {
  if (!deck.value) return
  model.value = null
  error.value = null

  let matches
  if (matches = deck.value.match(arkhamDbRegex)) {
    deckUrl.value = `${localizeArkhamDBBaseUrl()}/api/public/${matches[1]}/${matches[4]}`
    const response = await fetch(deckUrl.value)
    const data = await response.json()
    model.value = {...data, url: deckUrl.value}
  } else if (matches = deck.value.match(arkhamBuildRegex)) {
    if (/^[0-9]+$/.test(matches[4])) {
      deckUrl.value = `${localizeArkhamDBBaseUrl()}/api/public/${matches[1]}/${matches[4]}`
      const response = await fetch(deckUrl.value)
      const data = await response.json()
      model.value = {...data, url: deckUrl.value}
    } else {
      deckUrl.value = `https://api.arkham.build/v1/public/share/${matches[4]}`
      const response = await fetch(deckUrl.value)
      if (response.ok) {
        const data = await response.json()
        model.value = {...data, url: deckUrl.value}
      } else {
        error.value = "Could not find deck, please make sure you have created a public share."
      }
    }
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
