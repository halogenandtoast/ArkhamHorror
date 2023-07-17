<script lang="ts" setup>
import { ref } from 'vue'
import * as Investigator from '@/arkham/types/Investigator';
import {imgsrc} from '@/arkham/helpers';
import { fetchInvestigators, newDeck } from '@/arkham/api'

const ready = ref(false)
const emit = defineEmits(['newDeck'])

fetchInvestigators().then(async (response) => {
  investigators.value = response
  ready.value = true
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


const errors = ref([])
const investigatorError = ref<string | null>(null)
const investigator = ref<string | null>(null)
const investigators = ref<Investigator.Investigator[]>([])
const deck = ref<string | null>(null)
const deckId = ref<string | null>(null)
const deckName = ref<string | null>(null)
const deckUrl = ref<string | null>(null)

function loadDeck() {
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
      })
  } else {
    investigator.value = null
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
  }
}

function pasteDeck(evt: ClipboardEvent) {
  if (evt.clipboardData) {
    deck.value = evt.clipboardData.getData('text')
    loadDeck()
  }
}

async function createDeck() {
  errors.value = []
  if (deckId.value && deckName.value && deckUrl.value) {
    newDeck(deckId.value, deckName.value, deckUrl.value).then((newDeck) => {
      deckId.value = null
      deckName.value = null
      deckUrl.value = null
      investigator.value = null
      deck.value = null
      emit('newDeck', newDeck)
    }).catch((error) => {
      fetch("https://arkhamdb.com/api/public/cards/")
        .then((response) => response.json()).then((data) => {
          errors.value = error.response.data.map((error: UnimplementedCardError) => {
            const match = data.find((c: ArkhamDBCard) => c.code == error.contents.replace(/^c/, ''))
            if (match) {
              const { name, xp } = match
              return xp ? `${name} (${xp})` : name
            }
            return "Unknown card"
          })
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
        <input v-if="investigator" v-model="deckName" />
        <button :disabled="!investigator" @click.prevent="createDeck">Create</button>
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
