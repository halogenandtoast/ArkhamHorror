<script lang="ts" setup>
import { ref } from 'vue'
import {imgsrc} from '@/arkham/helpers';
import { fetchInvestigators, newDeck, validateDeck } from '@/arkham/api'
import { CardDef } from '@/arkham/types/CardDef';

const ready = ref(false)
const emit = defineEmits(['newDeck'])

fetchInvestigators().then(async (response) => {
  fetch("/cards.json").then(async (cardResponse) => {
    cards.value = await cardResponse.json()
    investigators.value = response
    ready.value = true
  })
})

interface UnimplementedCardError {
  tag: string
  contents: string
}

interface ArkhamDBCard {
  name: string
  code: string
  xp?: string
}

const cards = ref([])
const errors = ref([])
const valid = ref(false)
const investigatorError = ref<string | null>(null)
const investigator = ref<string | null>(null)
const investigators = ref<CardDef[]>([])
const deck = ref<string | null>(null)
const deckId = ref<string | null>(null)
const deckName = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const deckList = ref<string | null>(null)

function loadDeckFromFile(e) {
  valid.value = false
  const files = e.target.files || e.dataTransfer.files;
  const deck = files[0]
  if (deck) {
    const reader = new FileReader()
    reader.onload = (e) => {
      let data = JSON.parse(e.target.result)
      deckList.value = data
      investigator.value = null
      investigatorError.value = null
      if (investigators.value.map(i => i.art).includes(data.investigator_code)) {
        if(data.meta && data.meta.alternate_front) {
          investigator.value = data.meta.alternate_front
        } else {
          investigator.value = data.investigator_code
        }
      } else {
        investigatorError.value = `${data.investigator_name} is not yet implemented, please use a different deck`
      }
      deckId.value = data.id.toString()
      deckName.value = data.name

      runValidations()
    }
    reader.readAsText(deck)
  }
}

function loadDeck() {
  valid.value = false
  if (!deck.value) {
    return
  }

  const matches = deck.value.match(/\/(deck(list)?)(\/view)?\/([^/]+)/)
  if (matches) {
    deckUrl.value = `https://arkhamdb.com/api/public/${matches[1]}/${matches[4]}`
    fetch(deckUrl.value)
      .then((response) => response.json(), () => {
        investigator.value = null
        deckId.value = null
        deckName.value = null
        deckUrl.value = null
      })
      .then((data) => {
        deckList.value = data
        investigator.value = null
        investigatorError.value = null
        if (investigators.value.map(i => i.art).includes(data.investigator_code)) {
          if(data.meta && data.meta.alternate_front) {
            investigator.value = data.meta.alternate_front
          } else {
            investigator.value = data.investigator_code
          }
        } else {
          investigatorError.value = `${data.investigator_name} is not yet implemented, please use a different deck`
        }
        deckId.value = matches[4]
        deckName.value = data.name

        runValidations()
      })
  } else {
    investigator.value = null
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
  }
}

function runValidations() {
  valid.value = false
  errors.value = []
  validateDeck(deckList.value).then(() => valid.value = true).catch((error) => {
    errors.value = error.response.data.map((error: UnimplementedCardError) => {
      const match = cards.value.find((c: ArkhamDBCard) => c.code == error.contents.replace(/^c/, ''))
      if (match) {
        const { name, xp } = match
        return xp ? `${name} (${xp})` : name
      }
      return "Unknown card"
    })
  })
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text')
    loadDeck()
  }
}

async function createDeck() {
  errors.value = []
  if (deckId.value && deckName.value && valid.value) {
    newDeck(deckId.value, deckName.value, deckUrl.value, deckList.value).then((newDeck) => {
      deckId.value = null
      deckName.value = null
      deckUrl.value = null
      investigator.value = null
      deck.value = null
      emit('newDeck', newDeck)
    }).catch((error) => {
      errors.value = error.response.data.map((error: UnimplementedCardError) => {
        const match = cards.value.find((c: ArkhamDBCard) => c.code == error.contents.replace(/^c/, ''))
        if (match) {
          const { name, xp } = match
          return xp ? `${name} (${xp})` : name
        }
        return "Unknown card"
      })
    })
  }
}
</script>

<template>
  <div v-if="ready" class="new-deck">
    <div class="form-body">
      <img v-if="investigator" class="portrait" :src="imgsrc(`portraits/${investigator.replace('c', '')}.jpg`)" />
      <div class="fields">
        <input
          type="url"
          v-model="deck"
          @change="loadDeck"
          @paste.prevent="pasteDeck($event)"
          placeholder="ArkhamDB deck url"
        />
        <input type="file" @change="loadDeckFromFile" />
        <input v-if="investigator" v-model="deckName" />
        <button :disabled="!valid" @click.prevent="createDeck">Create</button>
      </div>
    </div>
    <div class="errors" v-if="investigatorError">
      {{investigatorError}}
    </div>
    <div class="errors" v-if="errors.length > 0">
      <p>Could not create deck, the following cards are unimplemented:</p>
      <ul>
        <li class="error" v-for="(error, idx) in errors" :key="idx">
          {{error}}
        </li>
      </ul>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.new-deck {
  input {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
  }
  .portrait {
    margin-right: 10px;
    height: 170px;
  }
  .fields {
    flex: 1;
    display: flex;
    flex-direction: column;
    flex-flow: wrap;
  }
  .errors {
    background-color: #660000;
    width: 100%;
    margin-top: 10px;
    padding: 15px;
    box-sizing: border-box;
  }
  button {
    outline: 0;
    padding: 15px;
    background: #6E8640;
    text-transform: uppercase;
    color: white;
    border: 0;
    width: 100%;
    &:hover {
      background: darken(#6E8640, 7%);
    }
  }
  button[disabled] {
    background: #999;
    cursor: not-allowed;
    &:hover {
      background: #999;
    }
  }
  display: flex;
  flex-direction: column;
  color: #FFF;
  background-color: #15192C;
  margin: 10px;
  padding: 10px;
  border-radius: 3px;
  a {
    color: #365488;
    font-weight: bolder;
  }
}

.form-body {
  display: flex;
}
</style>
