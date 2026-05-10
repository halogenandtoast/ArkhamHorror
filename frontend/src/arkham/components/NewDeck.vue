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
  alwaysSave?: boolean
  setPortrait?: (src: string) => void
}>(), { noPortrait: false, alwaysSave: false })

const emit = defineEmits(['newDeck', 'newDeckList'])

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
const saveDeck = computed(() => props.alwaysSave ? true : saveDeckToggle.value)
const saveDeckToggle = ref(true)
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

  if (!saveDeck.value && deckList.value) {
    const dl = deckList.value
    deckId.value = null
    deckName.value = null
    deckUrl.value = null
    investigator.value = null
    deck.value = null
    emit('newDeckList', dl)
    return
  }

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
        <div v-if="!alwaysSave" class="save-option" :class="{ active: saveDeck }" @click="saveDeckToggle = !saveDeckToggle" role="checkbox" :aria-checked="saveDeck">
          <div class="save-option-body">
            <span class="save-option-title">{{ $t('deckList.saveToDeckList') }}</span>
            <span class="save-option-desc">{{ $t('deckList.saveToDeckListDescription') }}</span>
          </div>
          <div class="save-option-toggle" :class="{ on: saveDeck }">
            <div class="save-option-thumb" />
          </div>
        </div>
        <button :disabled="!valid" @click.prevent="createDeck" class="primary-action">{{ alwaysSave ? 'Save' : saveDeck ? 'Save &amp; Use' : 'Use Without Saving' }}</button>
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

<style scoped>
.new-deck {
  :deep(input[type=url]) {
    margin-bottom: 0;
  }
  :deep(input) {
    outline: 0;
    border: 1px solid rgba(255,255,255,0.10);
    border-radius: 5px;
    padding: 12px 14px;
    color: #e0e0e0;
    background: var(--background-dark);
    width: 100%;
    font-size: 0.92em;
    transition: border-color 120ms ease;

    &:focus {
      border-color: rgba(110, 134, 64, 0.7);
    }
  }
  :deep(input[type=file]) {
    padding: 8px 12px;
    color: #888;
    cursor: pointer;

    &::file-selector-button {
      background: rgba(255,255,255,0.08);
      border: 1px solid rgba(255,255,255,0.15);
      border-radius: 4px;
      color: #ccc;
      padding: 5px 12px;
      font-size: 0.82em;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      cursor: pointer;
      margin-right: 10px;
      transition: background 150ms ease;

      &:hover {
        background: rgba(255,255,255,0.14);
      }
    }
  }
  .portrait {
    margin-right: 10px;
    height: 170px;
    border-radius: 5px;
  }
  .fields {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 10px;
  }
  .errors {
    background-color: rgba(100, 0, 0, 0.85);
    border: 1px solid rgba(255,80,80,0.2);
    border-radius: 6px;
    width: 100%;
    margin-top: 10px;
    padding: 14px 16px;
  }
  /* Save option toggle card */
  .save-option {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 12px 14px;
    border-radius: 6px;
    border: 1px solid rgba(255,255,255,0.10);
    background: rgba(255,255,255,0.04);
    cursor: pointer;
    user-select: none;
    transition: background 150ms ease, border-color 150ms ease;
    width: 100%;

    &:hover {
      background: rgba(255,255,255,0.08);
    }

    &.active {
      border-color: rgba(110, 134, 64, 0.6);
      background: rgba(110, 134, 64, 0.10);
    }
  }

  .save-option-body {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 2px;
  }

  .save-option-title {
    font-size: 0.88em;
    font-weight: 600;
    color: #ddd;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .save-option-desc {
    font-size: 0.76em;
    color: #888;
  }

  .save-option-toggle {
    width: 38px;
    height: 22px;
    border-radius: 11px;
    background: rgba(255,255,255,0.15);
    border: 1px solid rgba(255,255,255,0.15);
    position: relative;
    flex-shrink: 0;
    transition: background 200ms ease, border-color 200ms ease;

    &.on {
      background: rgba(110, 134, 64, 0.9);
      border-color: rgba(110, 134, 64, 0.6);
    }
  }

  .save-option-thumb {
    position: absolute;
    top: 2px;
    left: 2px;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: white;
    box-shadow: 0 1px 3px rgba(0,0,0,0.4);
    transition: transform 200ms ease;

    .save-option-toggle.on & {
      transform: translateX(16px);
    }
  }

  /* Primary action button */
  button.primary-action {
    outline: 0;
    width: 100%;
    height: 48px;
    border-radius: 5px;
    margin-top: 8px;
    border: 1px solid rgba(255,255,255,0.10);
    background: rgba(110, 134, 64, 0.95);
    color: white;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    font-size: 0.88em;
    cursor: pointer;
    box-shadow: 0 5px 18px rgba(0,0,0,0.3);
    transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease;

    &:hover:not(:disabled) {
      transform: translateY(-1px);
      background: rgba(110, 134, 64, 1);
      box-shadow: 0 10px 28px rgba(0,0,0,0.4);
    }

    &:active:not(:disabled) {
      transform: translateY(0);
    }

    &:disabled {
      opacity: 0.55;
      cursor: not-allowed;
      box-shadow: none;
      transform: none;
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
