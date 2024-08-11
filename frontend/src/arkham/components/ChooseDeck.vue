<script lang="ts" setup>
import { computed, ref, inject } from 'vue'
import type { Game } from '@/arkham/types/Game';
import { fetchDecks } from '@/arkham/api'
import { imgsrc } from '@/arkham/helpers'
import * as Arkham from '@/arkham/types/Deck'
import type { Investigator } from '@/arkham/types/Investigator'
import Question from '@/arkham/components/Question.vue';

const decks = ref<Arkham.Deck[]>([])
const ready = ref(false)
const deckId = ref<string | null>(null)

const props = defineProps<{
  game: Game
  playerId: string
}>()

const chooseDeck = inject<(deckId: string) => Promise<void>>('chooseDeck')
const question = computed(() => props.game.question[props.playerId])

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
  if(!deckId.value) {
    return true
  }

  return error.value !== null
})

const investigators = computed(() => props.game.investigators)

fetchDecks().then((result) => {
  decks.value = result;
  ready.value = true;
})

async function choose() {
  if (deckId.value && error.value === null) {
    if (chooseDeck) {
      await chooseDeck(deckId.value)
    }
  }
}

type Player = { tag: "EmptyPlayer", id: string } | { tag: "Chosen", contents: Investigator, id: string }

const tabooList = function (investigator: Investigator) {
  if (investigator.taboo) {
    switch (investigator.taboo) {
      case "TabooList15": return "1.5 (Apr 23, 2019)"
      case "TabooList16": return "1.6 (Sep 27, 2019)"
      case "TabooList18": return "1.8 (Oct 15, 2020)"
      case "TabooList19": return "1.9 (Jun 28, 2021)"
      case "TabooList20": return "2.0 (Aug 26, 2022)"
      case "TabooList21": return "2.1 (Aug 30, 2023)"
      case "TabooList22": return "2.2 (Feb 20, 2024)"
      default: return "Unknown Taboo List"
    }
  }

  return null
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
  if (!deck) {
    return null
  }

  if (deck.list.taboo_id) {
    switch (deck.list.taboo_id) {
      case 1: return "1.5 (Apr 23, 2019)"
      case 2: return "1.6 (Sep 27, 2019)"
      case 3: return "1.8 (Oct 15, 2020)"
      case 4: return "1.9 (Jun 28, 2021)"
      case 5: return "2.0 (Aug 26, 2022)"
      case 6: return "2.1 (Aug 30, 2023)"
      case 7: return "2.2 (Feb 20, 2024)"
      default: return "Unknown Taboo List"
    }
  }

  return null
})

</script>

<template>
  <div class="container">
    <div class="investigators">
      <h2>Choose your deck{{ players.length > 1 ? 's' : ''}}</h2>
      <div class="portraits">
        <div class="investigator-row" v-for="player in players" :key="player.id">
          <template v-if="player.tag === 'Chosen'">
            <div class="portrait">
              <img
                :src="portraitImage(player.investigator)"
              />
            </div>
            <div v-if="question && playerId == player.investigator.playerId" class="question">
              <Question :game="game" :playerId="playerId" @choose="choose" />
            </div>
            <div v-else>
              <div v-if="tabooList(player.investigator)" class="taboo-list">
                Taboo List: {{tabooList(player.investigator)}}
              </div>
            </div>
          </template>
          <template v-else>
            <div v-if="chosenImage && player.id == playerId" class="portrait">
              <img :src="chosenImage" />
            </div>
            <div v-else class="portrait portrait-empty">
              <img :src="imgsrc('slots/ally.png')" />
            </div>
            <div v-if="needsReply && player.id == playerId" class="deck-main">
              <form class="choose-deck" @submit.prevent="choose">
                <select v-model="deckId">
                  <option disabled :value="null">-- Select a Deck--</option>
                  <option v-for="deck in decks" :key="deck.id" :value="deck.id">{{deck.name}}</option>
                </select>
                <p class="error" v-if="error">{{error}}</p>
                <button type="submit" :disabled="disabled">Choose</button>
              </form>
              <div v-if="chosenDeckTabooList" class="taboo-list">
                Taboo List: {{chosenDeckTabooList}}
              </div>
            </div>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>


<style lang="scss" scoped>
.investigators {
  box-sizing: border-box;
  width: 100%;
  color: #FFF;
  padding: 10px;
  border-radius: 3px;
  max-width: 800px;
  margin-inline: auto;
  margin-top: 20px;

  h2 {
    margin: 0;
    padding: 0;
    text-transform: uppercase;
    color: white;
    margin-bottom: 10px;
  }
}

.portraits {
  --gap: 10px;
  --columns: 4;
  display: flex;
  flex-direction: column;
  gap: var(--gap);
}

.deck-main {
  width: 100%;
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.choose-deck {
  box-sizing: border-box;
  width: 100%;
  color: #FFF;
  border-radius: 3px;
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
  input[type=text] {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
  }
  select {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: var(--background-dark);
    width: 100%;
    box-sizing: border-box;
    margin-bottom: 10px;
    background-image:
      linear-gradient(45deg, transparent 50%, gray 50%),
      linear-gradient(135deg, gray 50%, transparent 50%),
      linear-gradient(to right, #ccc, #ccc);
    background-position:
      calc(100% - 25px) calc(1.3em + 2px),
      calc(100% - 20px) calc(1.3em + 2px),
      calc(100% - 3.5em) 0.5em;
    background-size:
      5px 5px,
      5px 5px,
      1px 2.5em;
    background-repeat: no-repeat;
  }
  a {
    color: #365488;
    font-weight: bolder;
  }
  p {
    margin: 0;
    padding: 0;
    text-transform: uppercase;
  }
}

h2 {
  color: #656A84;
  margin-left: 10px;
  text-transform: uppercase;
}

input[type=radio] {
  display: none;
  /* margin: 10px; */
}

input[type=radio] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: desaturate(#6E8640, 30%);
  &:hover {
    background-color: desaturate(#6E8640, 20%);
  }
  border-color: #ddd;
}

input[type=radio]:checked + label {
  background: #6E8640;
}

input[type=checkbox] {
  display: none;
  /* margin: 10px; */
}

input[type=checkbox] + label {
  display:inline-block;
  padding: 4px 12px;
  background-color: desaturate(#6E8640, 30%);
  &:hover {
    background-color: desaturate(#6E8640, 20%);
  }

  border-color: #ddd;
}

input[type=checkbox]:checked + label {
  background: #6E8640;
}

header {
  display: flex;
  align-items: center;
  justify-items: center;
  align-content: center;
  justify-content: center;
}

select::-ms-expand {
  display: none;
}

select {
  appearance: none;
  -moz-appearance: none;
  -webkit-appearance: none;
}

.container {
  background: var(--background);
  width: 100%;
  max-width: unset;
  height: 100%;
  margin: 0;
}

form {
  max-width: 800px;
  margin-inline: auto;
  margin-top: 20px;
}

.choose-deck {
  p.error {
    color: white;
    background-color: darkred;
    padding: 10px;
    text-align: center;
    margin-bottom: 10px;
    display: block;
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
  aspect-ratio: 63/97;
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  background: rgba(100, 100, 100, 0.5);
  display: flex;
  align-items: center;
  align-content: center;
  justify-content: center;
  justify-items: center;
  img {
    width: 80%;
  }
}

.investigator-row {
  padding: 10px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 10px;
  display: flex;
  gap: 10px;
  justify-items: flex-start;
  & :deep(.choices) {
    margin: 0;
    padding: 0;
  }
  & :deep(form) {
    margin: 0px;
    height: fit-content;
  }
  & :deep(.choose-deck) {
    border-radius: 5px;
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

.taboo-list {
  color: #A8A749;
  margin: auto;
  padding: 5px 10px;
  border-radius: 3px;
  font-weight: bold;
  text-transform: uppercase;
  width: 100%;
  box-sizing: border-box;
}
</style>
