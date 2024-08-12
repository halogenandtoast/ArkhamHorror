<script lang="ts" setup>
import { watch, ref, onMounted } from 'vue'
import {imgsrc} from '@/arkham/helpers';
import { fetchInvestigators, newDeck, validateDeck } from '@/arkham/api'
import { CardDef } from '@/arkham/types/CardDef';
import ArkhamDbDeck from '@/arkham/components/ArkhamDbDeck.vue';

type Props = {
  noPortrait?: boolean
  setPortrait?: (src: string) => void
}

const props = withDefaults(defineProps<Props>(), {
  noPortrait: false
})

const ready = ref(false)
const emit = defineEmits(['newDeck'])

onMounted(async () => {
  fetchInvestigators().then(async (response) => {
    fetch("/cards.json").then(async (cardResponse) => {
      cards.value = await cardResponse.json()
      investigators.value = response
      ready.value = true
    })
  })
})

interface Meta {
  alternate_front: string
}

interface ArkhamDbDecklist {
  id: string
  url: string | null
  meta?: Meta
  name: string
  investigator_code: string
  investigator_name: string
  slots: {
    [key: string]: number
  }
}

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
const deckList = ref<ArkhamDbDecklist | null>(null)

function loadDeckFromFile(e: Event) {
  valid.value = false
  const files = (e.target as HTMLInputElement).files || (e as DragEvent).dataTransfer?.files || [];
  const deck = files[0]
  if (deck) {
    const reader = new FileReader()
    reader.onloadend = (e1: ProgressEvent<FileReader>) => {
      if(!e1?.target?.result) return
      let data = JSON.parse(e1.target.result.toString())
      deckList.value = data
      investigator.value = null
      investigatorError.value = null
      if (investigators.value.includes(data.investigator_code)) {
        if(data.meta && data.meta.alternate_front) {
          investigator.value = data.meta.alternate_front
          if (props.setPortrait) {
            props.setPortrait(imgsrc(`portraits/${data.meta.alternate_front.replace('c', '')}.jpg`))
          }
        } else {
          investigator.value = data.investigator_code
          if (props.setPortrait) {
            props.setPortrait(imgsrc(`portraits/${data.investigator_code.replace('c', '')}.jpg`))
          }
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

watch(deckList, loadDeck)

function loadDeck() {
  valid.value = false
  if (!deckList.value) {
    return
  }

  investigator.value = null
  investigatorError.value = null
  if (investigators.value.includes(deckList.value.investigator_code)) {
    if(deckList.value.meta && deckList.value.meta.alternate_front) {
      investigator.value = deckList.value.meta.alternate_front
      if (props.setPortrait) {
        props.setPortrait(imgsrc(`portraits/${deckList.value.meta.alternate_front.replace('c', '')}.jpg`))
      }
    } else {
      investigator.value = deckList.value.investigator_code
      if (props.setPortrait) {
        props.setPortrait(imgsrc(`portraits/${deckList.value.investigator_code.replace('c', '')}.jpg`))
      }
    }
  } else {
    investigatorError.value = `${deckList.value.investigator_name} is not yet implemented, please use a different deck`
  }
  deckId.value = String(deckList.value.id)
  deckName.value = deckList.value.name
  deckUrl.value = deckList.value.url

  runValidations()
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
  <div class="new-deck">
    <div class="form-body">
      <img v-if="investigator && !noPortrait" class="portrait" :src="imgsrc(`portraits/${investigator.replace('c', '')}.jpg`)" />
      <div class="fields">
        <ArkhamDbDeck type="url" v-model="deckList" />
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
    border: 1px solid var(--background);
    padding: 15px;
    color: #F2F2F2;
    background: var(--background-dark);
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
