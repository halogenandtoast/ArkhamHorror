<script lang="ts" setup>
import { computed, watch, ref } from 'vue'
import { storeToRefs } from 'pinia'
import {imgsrc} from '@/arkham/helpers';
import { fetchInvestigators, newDeck, validateDeck } from '@/arkham/api'
import ArkhamDbDeck from '@/arkham/components/ArkhamDbDeck.vue';
import { ArkhamDbDecklist } from '@/arkham/types/Deck';
import { useCardStore } from '@/stores/cards'

const props = withDefaults(defineProps<{
  noPortrait?: boolean
  setPortrait?: (src: string) => void
}>(), { noPortrait: false })

const emit = defineEmits(['newDeck'])

const store = useCardStore()
const investigators = ref<string[]>([])
const { cards }  = storeToRefs(store)

investigators.value = await fetchInvestigators()

interface UnimplementedCardError {
  tag: string
  contents: string
}

interface ArkhamDBCard {
  name: string
  code: string
  xp?: string
}

const errors = ref<string[]>([])
const valid = ref(false)
const investigatorError = ref<string | null>(null)
const investigator = ref<string | null>(null)
const deck = ref<string | null>(null)
const deckId = ref<string | null>(null)
const deckName = ref<string | null>(null)
const deckUrl = ref<string | null>(null)
const deckList = ref<ArkhamDbDecklist | null>(null)
const maybeSetPortrait = (code: string | null | undefined) => {
  if (!code || !props.setPortrait) return
  props.setPortrait(imgsrc(`portraits/${normalizeCode(code)}.jpg`))
}
const resolvedInvestigatorCode = (d: ArkhamDbDecklist) =>
  d.meta?.alternate_front ?? d.investigator_code

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
        investigatorError.value = `${data.investigator_name} is not yet implemented, please use a different deck ${data}`
      }
      deckId.value = data.id.toString()
      deckName.value = data.name

      runValidations()
    }
    reader.readAsText(deck)
  }
}

watch(deckList, loadDeck)

async function loadDeck() {
  valid.value = false
  errors.value = []
  investigator.value = null
  investigatorError.value = null

  const dl = deckList.value
  if (!dl) return

  const invCode = resolvedInvestigatorCode(dl)
  const invImplemented = investigators.value.includes(dl.investigator_code)

  if (invImplemented) {
    investigator.value = invCode
    maybeSetPortrait(invCode)
  } else {
    investigatorError.value =
      `${dl.investigator_name} is not yet implemented, please use a different deck ${JSON.stringify(dl)}`
  }

  deckId.value = String(dl.id)
  deckName.value = dl.name
  deckUrl.value = dl.url

  await runValidations()
}

const normalizeCode = (code: string) => code.replace(/^c/, '')
const cardByCode = computed(() => {
  const m = new Map<string, ArkhamDBCard>()
  for (const c of cards.value) m.set(normalizeCode(c.code), c)
  return m
})

async function runValidations() {
  valid.value = false
  errors.value = []
  try {
    await validateDeck(deckList.value)
    valid.value = true
  } catch (err: any) {
    const payload: UnimplementedCardError[] = err?.response?.data ?? []
    errors.value = payload.map((e) => {
      const key = normalizeCode(e.contents)
      const hit = cardByCode.value.get(key)
      if (hit) return hit.xp ? `${hit.name} (${hit.xp})` : hit.name
      return `Unknown card: ${e.contents}`
    })
  }
}

async function createDeck() {
  errors.value = []
  if (!(deckId.value && deckName.value && valid.value)) return

  try {
    const created = await newDeck(deckId.value, deckName.value, deckUrl.value, deckList.value)
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
    investigator.value = null
    deck.value = null
    emit('newDeck', created)
  } catch (err: any) {
    const payload: UnimplementedCardError[] = err?.response?.data ?? []
    errors.value = payload.map((e) => {
      const key = normalizeCode(e.contents)
      const hit = cardByCode.value.get(key)
      if (hit) return hit.xp ? `${hit.name} (${hit.xp})` : hit.name
      return 'Unknown card'
    })
  }
}
</script>

<template>
  <div class="new-deck">
    <div class="form-body">
      <img v-if="investigator && !noPortrait" class="portrait" :src="imgsrc(`portraits/${investigator.replace('c', '')}.jpg`)" />
      <div class="fields">
        <ArkhamDbDeck v-model="deckList" />
        <input type="file" accept=".json,application/json" @change="loadDeckFromFile" />
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
  :deep(input) {
    outline: 0;
    border: 1px solid var(--background);
    padding: 15px;
    color: #F2F2F2;
    background: var(--background-dark);
    width: 100%;
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
      background: hsl(80, 35%, 32%);
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
