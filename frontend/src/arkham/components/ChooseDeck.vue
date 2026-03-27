<script lang="ts" setup>
import { displayTabooId, displayTabooList } from '@/arkham/taboo';
import { computed, ref, inject } from 'vue'
import type { Game } from '@/arkham/types/Game';
import { fetchDecks } from '@/arkham/api'
import { imgsrc } from '@/arkham/helpers'
import * as Arkham from '@/arkham/types/Deck'
import {deckClass} from '@/arkham/types/Deck'
import type { ArkhamDbDecklist } from '@/arkham/types/Deck'
import type { Investigator } from '@/arkham/types/Investigator'
import Question from '@/arkham/components/Question.vue';
import NewDeck from '@/arkham/components/NewDeck.vue'

const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const deckId = ref<string | null>(null)
const unsavedDeckList = ref<ArkhamDbDecklist | null>(null)
const createdPortrait = ref<string | null>(null)

type DeckType = "UseExistingDeck" | "LoadNewDeck" | "UnsavedDeck"
const deckType = ref<DeckType>("UseExistingDeck")

const props = defineProps<{
  game: Game
  playerId: string
}>()

const chooseDeck = inject<(deckId: string) => Promise<void>>('chooseDeck')
const chooseDeckList = inject<(deckList: ArkhamDbDecklist) => Promise<void>>('chooseDeckList')
const question = computed(() => props.game.question[props.playerId])

const questionLabel = computed(() => {
  if (question.value)
    return question.value.tag === 'QuestionLabel' ? question.value.label : null
})

async function setPortrait(src: string) {
  createdPortrait.value = src
}

async function addDeck(d: Arkham.Deck) {
  decks.value = [...decks.value, d]
  deckId.value = d.id
  unsavedDeckList.value = null
  deckType.value = "UseExistingDeck"
}

async function addUnsavedDeck(dl: ArkhamDbDecklist) {
  unsavedDeckList.value = dl
  deckId.value = null
  deckType.value = "UnsavedDeck"
}

const error = computed(() => {
  if(!deckId.value) {
    return null
  }

  const deck = decks.value.find((d) => d.id === deckId.value)
  if (!deck) {
    return null
  }

  const alreadyTaken = Object.values(props.game.investigators).some((i) => {
    return i.id === deck.list.investigator_code
  })

  if (alreadyTaken) {
    return 'This investigator is already taken'
  }

  const inOtherScenario = Object.values(props.game.otherInvestigators).some((i) => {
    return i.id === deck.list.investigator_code
  })

  if (inOtherScenario) {
    return 'This investigator is already taken in this campaign'
  }

  return null
})

const disabled = computed(() => {
  if (unsavedDeckList.value) return false
  if (!deckId.value) return true
  return error.value !== null
})

const investigators = computed(() => props.game.investigators)

fetchDecks().then((result) => {
  decks.value = result;
  if (result.length == 0) {
    deckType.value = "LoadNewDeck"
  }
  ready.value = true;
})

const emit = defineEmits(['choose'])

const chooseChoice = (idx: number) => emit('choose', idx)

async function choose() {
  if (unsavedDeckList.value && chooseDeckList) {
    await chooseDeckList(unsavedDeckList.value)
  } else if (deckId.value && error.value === null) {
    if (chooseDeck) {
      await chooseDeck(deckId.value)
    }
  }
}

type Player = { tag: "EmptyPlayer", id: string } | { tag: "Chosen", contents: Investigator, id: string }

const tabooList = function (investigator: Investigator) {
  return investigator.taboo ? displayTabooList(investigator.taboo) : null
}

const players = computed<Player[]>(() => {
  if (props.game.gameState.tag === 'IsChooseDecks') {
    return props.game.gameState.contents.map((p) => {
      const maybeInvestigator = Object.values(investigators.value).find((i) => i.playerId === p)
      return maybeInvestigator ? { tag: "Chosen", investigator: maybeInvestigator, id: p } : { tag: "EmptyPlayer", id: p }
    })
  }

  return []
})

function portraitImage(investigator: Investigator) {
  return imgsrc(`portraits/${investigator.cardCode.replace('c', '')}.jpg`)
}

const needsReply = computed(() => {
  const question = props.game.question[props.playerId]
  if (question === null || question === undefined) {
    return false
  }

  return question.tag === 'ChooseDeck' || (question.tag === 'QuestionLabel' && question.question.tag === 'ChooseDeck')
})

const chosenImage = computed(() => {
  if(!deckId.value) {
    return null
  }

  const deck = decks.value.find((d) => d.id === deckId.value)
  if (!deck) {
    return null
  }
  
  if (deck.list.meta) {
    try {
      const result = JSON.parse(deck.list.meta)
      if (result && result.alternate_front) {
        return imgsrc(`portraits/${result.alternate_front}.jpg`)
      }
    } catch (e) { console.log("No parse") }
  }

  return imgsrc(`portraits/${deck.list.investigator_code.replace('c', '')}.jpg`)
})

const chosenDeckTabooList = computed(() => {
  if(!deckId.value) {
    return null
  }

  const deck = decks.value.find((d) => d.id === deckId.value)
  if (!deck) return null

  return deck.list.taboo_id ? displayTabooId(deck.list.taboo_id) : null
})

</script>

<template>
  <div class="container">
    <div class="investigators">
      <h2 class="page-title">{{$t('create.chooseYourDeck', {s: players.length > 1 ? 's' : ''})}}</h2>
      <div class="portraits">
        <div class="investigator-row" v-for="player in players" :key="player.id">
          <template v-if="player.tag === 'Chosen'">
            <div class="portrait">
              <img :src="portraitImage(player.investigator)" />
            </div>
            <div v-if="question && playerId == player.investigator.playerId" class="question">
              <h2 v-if="questionLabel" class="title question-label">{{ questionLabel }}</h2>
              <Question :game="game" :playerId="playerId" @choose="chooseChoice" />
            </div>
            <div v-else>
              <div v-if="tabooList(player.investigator)" class="taboo-list">
                {{$t('create.tabooList', {tabooList: tabooList(player.investigator)})}}
              </div>
            </div>
          </template>
          <template v-else>
            <div v-if="chosenImage && player.id == playerId" class="portrait">
              <img :src="chosenImage" />
            </div>
            <div v-else-if="createdPortrait && (deckType == 'LoadNewDeck' || deckType == 'UnsavedDeck')" class="portrait">
              <img :src="createdPortrait" />
            </div>
            <div v-else class="portrait-empty">
              <img :src="imgsrc('slots/ally.png')" />
            </div>
            <div v-if="needsReply && player.id == playerId" class="deck-main">
              <div class="deck-tabs">
                <button @click.prevent="deckType = 'UseExistingDeck'" :class="{ current: deckType == 'UseExistingDeck'}" :disabled="decks.length == 0">
                  {{$t('create.useExistingDeck')}}
                </button>
                <button @click.prevent="deckType = 'LoadNewDeck'" :class="{ current: deckType == 'LoadNewDeck' || deckType == 'UnsavedDeck'}">
                  {{$t('create.loadNewDeck')}}
                </button>
              </div>
              <form v-if="deckType == 'UseExistingDeck'" class="deck-form" @submit.prevent="choose">
                <div class="select-wrapper">
                  <select v-model="deckId">
                    <option disabled :value="null">{{$t('create.selectADeck')}}</option>
                    <option v-for="deck in decks" :key="deck.id" :value="deck.id" :class="deckClass(deck)">{{deck.name}}</option>
                  </select>
                  <svg class="select-chevron" viewBox="0 0 10 6" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M1 1l4 4 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>
                </div>
                <p class="error" v-if="error">{{error}}</p>
                <div v-if="chosenDeckTabooList" class="taboo-badge">
                  <font-awesome-icon icon="shield-halved" />
                  {{chosenDeckTabooList}}
                </div>
                <button type="submit" class="primary-action" :disabled="disabled">{{$t('create.choose')}}</button>
              </form>
              <form v-else-if="deckType == 'UnsavedDeck'" class="deck-form" @submit.prevent="choose">
                <p class="unsaved-deck-name">{{ unsavedDeckList?.name }}</p>
                <button type="submit" class="primary-action">{{$t('create.choose')}}</button>
              </form>
              <NewDeck v-else @new-deck="addDeck" @new-deck-list="addUnsavedDeck" :no-portrait="true" :set-portrait="setPortrait" />
            </div>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>


<style scoped>
.container {
  background: var(--background);
  width: 100%;
  max-width: unset;
  height: 100%;
  margin: 0;
}

.investigators {
  width: 100%;
  color: #FFF;
  padding: 10px;
  border-radius: 3px;
  max-width: 800px;
  margin-inline: auto;
  margin-top: 20px;
}

.page-title {
  margin: 0 0 12px 0;
  padding: 0;
  text-transform: uppercase;
  color: #cecece;
  font-family: Teutonic;
  font-size: 1.8em;
  letter-spacing: 0.04em;
}

.portraits {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.investigator-row {
  padding: 12px;
  background: rgba(255, 255, 255, 0.07);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 10px;
  display: flex;
  gap: 12px;
  align-items: flex-start;

  & :deep(.choices) {
    margin: 0;
    padding: 0;
  }
  & :deep(form) {
    margin: 0;
    height: fit-content;
  }

  .question {
    flex: 1;
    & :deep(.modal-contents) {
      border-radius: 5px;
      form {
        width: 100%;
        align-items: flex-start;
        display: flex;
        flex-direction: column;
        gap: 15px;
        label {
          text-transform: uppercase;
          margin-right: 15px;
        }
        button {
          width: 100%;
          margin: 0;
        }
      }
    }
  }
}

.portrait {
  width: 100px;
  border-radius: 5px;
  flex-shrink: 0;
  img {
    width: 100%;
    border-radius: 5px;
    box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  }
}

.portrait-empty {
  width: 100px;
  height: 155px;
  border-radius: 5px;
  flex-shrink: 0;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  background: rgba(100, 100, 100, 0.3);
  display: flex;
  align-items: center;
  justify-content: center;
  img {
    width: 80%;
    opacity: 0.6;
  }
}

.deck-main {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 10px;
}

/* Tab buttons */
.deck-tabs {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  gap: 8px;

  button {
    height: 42px;
    border-radius: 5px;
    border: 1px solid rgba(255,255,255,0.10);
    background: rgba(0,0,0,0.22);
    color: rgba(255,255,255,0.75);
    letter-spacing: 0.06em;
    text-transform: uppercase;
    font-size: 0.78em;
    cursor: pointer;
    transition: transform 120ms ease, background 160ms ease, color 120ms ease;
    outline: none;

    &:hover:not(:disabled) {
      background: rgba(255,255,255,0.08);
      color: rgba(255,255,255,0.95);
      transform: translateY(-1px);
    }

    &.current {
      background: rgba(110, 134, 64, 0.95);
      border-color: rgba(255,255,255,0.10);
      color: white;
    }

    &:disabled {
      opacity: 0.35;
      cursor: not-allowed;
    }
  }
}

/* Deck selection form */
.deck-form {
  display: flex;
  flex-direction: column;
  gap: 10px;

  p {
    margin: 0;
    padding: 0;
  }

  p.unsaved-deck-name {
    color: white;
    padding: 12px 16px;
    background: rgba(255,255,255,0.06);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 6px;
    text-align: center;
    font-weight: 600;
    letter-spacing: 0.04em;
  }

  p.error {
    color: white;
    background-color: rgba(180, 30, 30, 0.85);
    border: 1px solid rgba(255,80,80,0.3);
    border-radius: 5px;
    padding: 10px 14px;
    text-align: center;
    text-transform: uppercase;
    font-size: 0.82em;
    letter-spacing: 0.04em;
  }
}

/* Select wrapper with SVG chevron */
.select-wrapper {
  position: relative;

  select {
    appearance: none;
    -webkit-appearance: none;
    outline: 0;
    border: 1px solid rgba(255,255,255,0.12);
    border-radius: 5px;
    padding: 12px 40px 12px 14px;
    background: var(--background-dark);
    color: #e0e0e0;
    width: 100%;
    font-size: 0.92em;
    cursor: pointer;
    transition: border-color 120ms ease;

    &:focus {
      border-color: rgba(110, 134, 64, 0.7);
    }
  }

  .select-chevron {
    position: absolute;
    right: 14px;
    top: 50%;
    transform: translateY(-50%);
    width: 12px;
    height: 8px;
    color: #888;
    pointer-events: none;
  }
}

/* Primary action button */
.primary-action {
  width: 100%;
  height: 48px;
  border-radius: 5px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(110, 134, 64, 0.95);
  color: white;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  font-size: 0.88em;
  cursor: pointer;
  box-shadow: 0 5px 18px rgba(0,0,0,0.3);
  transition: transform 120ms ease, background 160ms ease, box-shadow 160ms ease;
  outline: none;

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

/* Taboo badge */
.taboo-badge {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  font-size: 0.72em;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: #A8A749;
  background: rgba(168, 167, 73, 0.12);
  border: 1px solid rgba(168, 167, 73, 0.3);
  border-radius: 4px;
  padding: 4px 10px;
  align-self: flex-start;
}

/* Taboo shown on chosen investigator rows */
.taboo-list {
  color: #A8A749;
  font-size: 0.78em;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  padding: 4px 0;
}

option.guardian {
  background-color: var(--guardian-dark);
}

option.mystic {
  background-color: var(--mystic-dark);
}
</style>
