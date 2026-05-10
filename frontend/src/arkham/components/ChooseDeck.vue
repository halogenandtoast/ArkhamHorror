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
import DeckToolbar from '@/arkham/components/DeckToolbar.vue'

const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const deckId = ref<string | null>(null)
const unsavedDeckList = ref<ArkhamDbDecklist | null>(null)
const createdPortrait = ref<string | null>(null)

type DeckType = "UseExistingDeck" | "LoadNewDeck" | "UnsavedDeck"
const deckType = ref<DeckType>("UseExistingDeck")

const searchText = ref('')
const filterClasses = ref<string[]>([])
const sortBy = ref<'name' | 'class'>('name')
const CLASS_ORDER: Record<string, number> = {
  guardian: 0, seeker: 1, rogue: 2, mystic: 3, survivor: 4, neutral: 5
}
const allClasses = ["guardian", "seeker", "rogue", "mystic", "survivor", "neutral"]

function deckInvestigatorCode(deck: Arkham.Deck): string {
  if (deck.list.meta) {
    try {
      const result = JSON.parse(deck.list.meta)
      if (result?.alternate_front) return result.alternate_front.replace('c', '')
    } catch (e) {}
  }
  return deck.list.investigator_code.replace('c', '')
}

function deckTaboo(deck: Arkham.Deck): string | null {
  return deck.list.taboo_id ? displayTabooId(deck.list.taboo_id) : null
}

const filteredDecks = computed(() => {
  let result = decks.value.filter((deck) => {
    const cls = deckClass(deck)
    const matchesClass = filterClasses.value.length === 0 ||
      filterClasses.value.some((k) => cls[k])
    const matchesSearch = !searchText.value ||
      deck.name.toLowerCase().includes(searchText.value.toLowerCase())
    return matchesClass && matchesSearch
  })

  if (sortBy.value === 'name') {
    result = [...result].sort((a, b) => a.name.localeCompare(b.name))
  } else if (sortBy.value === 'class') {
    result = [...result].sort((a, b) => {
      const ca = allClasses.find(k => (deckClass(a) as any)[k]) ?? 'neutral'
      const cb = allClasses.find(k => (deckClass(b) as any)[k]) ?? 'neutral'
      return (CLASS_ORDER[ca] ?? 5) - (CLASS_ORDER[cb] ?? 5)
    })
  }

  return result
})

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
  await choose()
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

async function selectAndChoose(deck: Arkham.Deck) {
  deckId.value = deck.id
  if (error.value !== null) return
  await choose()
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
            <div v-if="needsReply && player.id == playerId" class="deck-main">
              <div class="deck-tabs">
                <button @click.prevent="deckType = 'UseExistingDeck'" :class="{ current: deckType == 'UseExistingDeck'}" :disabled="decks.length == 0">
                  {{$t('create.useExistingDeck')}}
                </button>
                <button @click.prevent="deckType = 'LoadNewDeck'" :class="{ current: deckType == 'LoadNewDeck' || deckType == 'UnsavedDeck'}">
                  {{$t('create.loadNewDeck')}}
                </button>
              </div>
              <div v-if="deckType == 'UseExistingDeck'" class="deck-picker">
                <DeckToolbar
                  compact
                  :search-placeholder="$t('chooseDeck.search')"
                  v-model:search="searchText"
                  v-model:filterClasses="filterClasses"
                  v-model:sortBy="sortBy"
                />
                <div class="deck-list">
                  <div v-if="filteredDecks.length === 0" class="deck-list-empty">{{ $t('noDecksMatchFilters') }}</div>
                  <div
                    v-for="deck in filteredDecks"
                    :key="deck.id"
                    class="deck-item"
                    :class="[deckClass(deck), { selected: deckId === deck.id, 'has-error': deckId === deck.id && error }]"
                    @click.prevent="deckId = deck.id"
                  >
                    <img class="deck-item-portrait" :src="imgsrc(`cards/${deckInvestigatorCode(deck)}.avif`)" />
                    <div class="deck-item-info">
                      <span class="deck-item-name">{{ deck.name }}</span>
                      <span v-if="deckTaboo(deck)" class="deck-item-taboo">
                        <font-awesome-icon icon="book" /> {{ deckTaboo(deck) }}
                      </span>
                      <span v-if="deckId === deck.id && error" class="deck-item-error">{{ error }}</span>
                    </div>
                    <button class="deck-item-use" @click.stop.prevent="selectAndChoose(deck)" :title="$t('chooseDeck.useThisDeck')">
                      <font-awesome-icon icon="chevron-right" />
                    </button>
                  </div>
                </div>
              </div>
              <div v-else class="load-deck-layout">
                <div class="load-deck-portrait">
                  <div v-if="createdPortrait" class="portrait">
                    <img :src="createdPortrait" />
                  </div>
                  <div v-else class="portrait-empty">
                    <img :src="imgsrc('slots/ally.png')" />
                  </div>
                </div>
                <div class="load-deck-content">
                  <form v-if="deckType == 'UnsavedDeck'" class="deck-form" @submit.prevent="choose">
                    <p class="unsaved-deck-name">{{ unsavedDeckList?.name }}</p>
                    <button type="submit" class="primary-action">{{$t('create.choose')}}</button>
                  </form>
                  <NewDeck v-else @new-deck="addDeck" @new-deck-list="addUnsavedDeck" :no-portrait="true" :set-portrait="setPortrait" />
                </div>
              </div>
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

/* Tab buttons — segmented control */
.deck-tabs {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  gap: 3px;
  padding: 3px;
  background: rgba(0,0,0,0.30);
  border: 1px solid rgba(255,255,255,0.08);
  border-radius: 8px;

  button {
    height: 36px;
    border-radius: 6px;
    border: none;
    background: transparent;
    color: rgba(255,255,255,0.50);
    letter-spacing: 0.06em;
    text-transform: uppercase;
    font-size: 0.74em;
    cursor: pointer;
    transition: background 160ms ease, color 120ms ease, box-shadow 160ms ease;
    outline: none;

    &:hover:not(:disabled):not(.current) {
      background: rgba(255,255,255,0.06);
      color: rgba(255,255,255,0.80);
    }

    &.current {
      background: rgba(110, 134, 64, 0.88);
      color: white;
      box-shadow: 0 1px 4px rgba(0,0,0,0.35);
    }

    &:disabled {
      opacity: 0.30;
      cursor: not-allowed;
    }
  }
}

/* Deck picker (UseExistingDeck) */
.deck-picker {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.deck-list {
  display: flex;
  flex-direction: column;
  gap: 5px;
  max-height: calc(100dvh - 380px);
  min-height: 120px;
  overflow-y: auto;
  scrollbar-width: thin;
  scrollbar-color: rgba(255,255,255,0.15) transparent;
}

.deck-list-empty {
  padding: 24px;
  text-align: center;
  color: #555;
  font-size: 0.85em;
}

.deck-item {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 10px 12px;
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.06);
  border-left: 3px solid transparent;
  border-radius: 6px;
  cursor: pointer;
  transition: background 0.12s, border-color 0.12s;
  color: #e0e0e0;

  &:hover { background: rgba(255,255,255,0.08); }

  &.guardian { border-left-color: var(--guardian-dark); &:hover { background: var(--guardian-extra-dark); } }
  &.seeker   { border-left-color: var(--seeker-dark);   &:hover { background: var(--seeker-extra-dark); } }
  &.rogue    { border-left-color: var(--rogue-dark);    &:hover { background: var(--rogue-extra-dark); } }
  &.mystic   { border-left-color: var(--mystic-dark);   &:hover { background: var(--mystic-extra-dark); } }
  &.survivor { border-left-color: var(--survivor-dark); &:hover { background: var(--survivor-extra-dark); } }
  &.neutral  { border-left-color: var(--neutral-dark);  &:hover { background: var(--neutral-extra-dark); } }

  &.selected {
    border-color: rgba(110, 134, 64, 0.4);
    border-left-color: rgba(110, 134, 64, 0.9);
    background: rgba(110, 134, 64, 0.10);
  }

  &.has-error {
    border-color: rgba(200, 50, 50, 0.5);
    border-left-color: rgba(200, 50, 50, 0.9);
    background: rgba(160, 25, 25, 0.15);
  }
}

.deck-item-portrait {
  width: 60px;
  border-radius: 4px;
  flex-shrink: 0;
  box-shadow: 1px 1px 5px rgba(0,0,0,0.5);
}

.deck-item-info {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.deck-item-name {
  font-size: 0.94em;
  font-weight: 600;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.deck-item-taboo {
  font-size: 0.72em;
  font-weight: 600;
  color: #c8a96e;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.deck-item-error {
  font-size: 0.75em;
  color: #ff8080;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

.deck-item-use {
  flex-shrink: 0;
  width: 34px;
  height: 34px;
  border-radius: 5px;
  border: 1px solid rgba(255,255,255,0.10);
  background: rgba(110, 134, 64, 0.85);
  color: white;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.85em;
  transition: background 150ms ease, transform 120ms ease, box-shadow 150ms ease;
  outline: none;

  &:hover {
    background: rgba(110, 134, 64, 1);
    transform: scale(1.08);
    box-shadow: 0 4px 12px rgba(0,0,0,0.35);
  }

  &:active { transform: scale(1.0); }
}

/* Load New Deck layout: portrait left, form right */
.load-deck-layout {
  display: flex;
  gap: 12px;
  align-items: flex-start;
}

.load-deck-portrait {
  flex-shrink: 0;
}

.load-deck-content {
  flex: 1;
  min-width: 0;
}

/* UnsavedDeck form */
.deck-form {
  display: flex;
  flex-direction: column;
  gap: 10px;

  p {
    margin: 0;
    padding: 0;
  }

  p.unsaved-deck-name {
    color: #e0e0e0;
    padding: 12px 16px;
    background: rgba(255,255,255,0.05);
    border: 1px solid rgba(255,255,255,0.10);
    border-radius: 6px;
    text-align: center;
    font-weight: 600;
    letter-spacing: 0.04em;
    font-size: 0.92em;
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

/* Taboo shown on chosen investigator rows */
.taboo-list {
  color: #A8A749;
  font-size: 0.78em;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  padding: 4px 0;
}
</style>
