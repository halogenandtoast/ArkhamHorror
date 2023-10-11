<script lang="ts" setup>
import { JsonDecoder } from 'ts.data.json';
import { useWebSocket, useClipboard } from '@vueuse/core'
import { reactive, ref, computed, provide, onUnmounted, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useUserStore } from '@/stores/user'
import * as Arkham from '@/arkham/types/Game'
import { imgsrc } from '@/arkham/helpers';
import { fetchGame, undoChoice } from '@/arkham/api'
import GameLog from '@/arkham/components/GameLog.vue'
import api from '@/api'
import CardView from '@/arkham/components/Card.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue'
import StandaloneScenario from '@/arkham/components/StandaloneScenario.vue'
import ScenarioSettings from '@/arkham/components/ScenarioSettings.vue'
import Campaign from '@/arkham/components/Campaign.vue'
import CampaignLog from '@/arkham/components/CampaignLog.vue'
import CampaignSettings from '@/arkham/components/CampaignSettings.vue'
import { Card, cardDecoder } from '@/arkham/types/Card'
import { TarotCard, tarotCardDecoder, tarotCardImage } from '@/arkham/types/TarotCard'
import { useCardStore } from '@/stores/cards'
import { onBeforeRouteLeave } from 'vue-router'
import { useDebug } from '@/arkham/debug'

export interface Props {
  gameId: string
  spectate?: boolean
}

const source = ref(`${window.location.href}/join`) // fix-syntax`

const props = withDefaults(defineProps<Props>(), { spectate: false })

const debug = useDebug()
const route = useRoute()
const store = useCardStore()
const { copy } = useClipboard({ source })

interface GameCard {
  title: string
  card: Card
}

const gameCardDecoder = JsonDecoder.object<GameCard>(
  {
    title: JsonDecoder.string,
    card: cardDecoder
  },
  'GameCard'
);


const gameCard = ref<GameCard | null>(null)
const tarotCards = ref<TarotCard[]>([])

const spectate = route.fullPath.endsWith('/spectate')
const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`
const spectatePrefix = spectate ? "/spectate" : ""

const userStore = useUserStore()
const socketError = ref(false)
const onError = () => socketError.value = true
const onConnected = () => socketError.value = false
const websocketUrl = `${baseURL}/api/v1/arkham/games/${props.gameId}${spectatePrefix}?token=${userStore.token}`.
  replace(/https/, 'wss').
  replace(/http/, 'ws')
const { data, send, close } = useWebSocket(websocketUrl, { autoReconnect: true, onError, onConnected })

onBeforeRouteLeave(() => close())
onUnmounted(() => close())

store.fetchCards()
const cards = computed(() => store.cards)

const ready = ref(false)
const solo = ref(false)
const game = ref<Arkham.Game | null>(null)
const playerId = ref<string | null>(null)
const gameLog = ref<readonly string[]>(Object.freeze([]))

const question = computed(() => playerId.value ? game.value?.question[playerId.value] : null)

async function undo() {
  resultQueue.value = []
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
  undoChoice(props.gameId)
}

const uiLock = ref<boolean>(false)

const resultQueue = ref<any>([])

const handleResult = (result) => {
  switch(result.tag) {
    case "GameMessage":
      gameLog.value = Object.freeze([...gameLog.value, result.contents])
      return
    case "GameTarot":
      if (uiLock.value) {
        resultQueue.value.push(result)
      } else {
        JsonDecoder.array(tarotCardDecoder, 'tarotCards').decodeToPromise(result.contents).then((r) => {
          uiLock.value = true
          tarotCards.value = r
        })
      }
      return
    case "GameCard":
      if (uiLock.value) {
        resultQueue.value.push(result)
      } else {
        gameCardDecoder.decodeToPromise(result).then((r) => {
          uiLock.value = true
          gameCard.value = r
        })
      }
      return
    case "GameUpdate":
      if (uiLock.value) {
        resultQueue.value.push(result)
        game.value = {...game.value, question: {}} as Arkham.Game
      } else {
        Arkham.gameDecoder.decodeToPromise(result.contents)
          .then((updatedGame) => {
            loadAllImages(updatedGame).then(() => {
              game.value = updatedGame
              gameLog.value = Object.freeze([...updatedGame.log])
              if (solo.value === true) {
                if (Object.keys(game.value.question).length == 1) {
                  playerId.value = Object.keys(game.value.question)[0]
                } else if (game.value.activePlayerId !== playerId.value) {
                  playerId.value = Object.keys(game.value.question)[0]
                }
              }
            })
          })
      }
      return
  }
}

const continueUI = () => {
  gameCard.value = null
  tarotCards.value = []
  uiLock.value = false
}

watch(data, async (newData) => {
  const result = JSON.parse(newData)
  handleResult(result)
})

watch(uiLock, async () => {
  if(!uiLock.value) {
    const r = resultQueue.value.shift()
    if (r) {
      handleResult(r)
    }
  }
})

function loadAllImages(game: Arkham.Game): Promise<void[]> {
  const images = Object.values(game.cards).map((card) => {
    return new Promise<void>((resolve, reject) => {
      const { cardCode, isFlipped } = card.contents
      const suffix = !props.revealed && isFlipped ? 'b' : ''
      const url = imgsrc(`cards/${cardCode.replace(/^c/, '')}${suffix}.jpg`)
      if (preloaded.includes(url)) return resolve()
      const img = new Image()
      img.src = url
      img.onload = () => {
        preloaded.push(url)
        resolve()
      }
      img.onerror = reject
      })
  })

  return Promise.all(images)
}

fetchGame(props.gameId, spectate).then(({ game: newGame, playerId: newPlayerId, multiplayerMode}) => {
  loadAllImages(newGame).then(() => {
    game.value = newGame
    solo.value = multiplayerMode === "Solo"
    gameLog.value = Object.freeze(newGame.log)
    playerId.value = newPlayerId
    ready.value = true
  })


  // Object.values(newGame.cards).forEach((card) => {
  //   const { cardCode, isFlipped } = card.contents
  //   const suffix = !props.revealed && isFlipped ? 'b' : ''
  //   const url = imgsrc(`cards/${cardCode.replace(/^c/, '')}${suffix}.jpg`)
  //   if (preloaded.includes(url)) return
  //   preload(url)
  // })
})

async function choose(idx: number) {
  if (idx !== -1 && game.value && !spectate) {
    send(JSON.stringify({tag: 'Answer', contents: { choice: idx , playerId: playerId.value } }))
  }
}

async function chooseDeck(deckId: string): Promise<void> {
  if(game.value && !spectate) {
    send(JSON.stringify({tag: 'DeckAnswer', deckId }))
  }
}

async function choosePaymentAmounts(amounts: Record<string, number>): Promise<void> {
  if(game.value && !spectate) {
    send(JSON.stringify({tag: 'PaymentAmountsAnswer', contents: { amounts } }))
  }
}

async function chooseAmounts(amounts: Record<string, number>): Promise<void> {
  if(game.value && !spectate) {
    send(JSON.stringify({tag: 'AmountsAnswer', contents: { amounts } }))
  }
}

function switchInvestigator (newPlayerId: string) { playerId.value = newPlayerId }

async function update(state: Arkham.Game) { game.value = state }

const preloaded = reactive([])

const preload = (img) => {
  const preloadLink = document.createElement("link");
  preloadLink.href = img;
  preloadLink.rel = "preload";
  preloadLink.as = "image";
  preloaded.push(img)
  document.head.appendChild(preloadLink);
}

function debugExport () {
  fetch(new Request(`${api.defaults.baseURL}/arkham/games/${props.gameId}/export`))
  .then(resp => resp.blob())
  .then(blob => {
    const url = window.URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.style.display = 'none'
    a.href = url
    // the filename you want
    a.download = 'arkham-debug.json'
    document.body.appendChild(a)
    a.click()
    window.URL.revokeObjectURL(url)
  })
  .catch((e) => {
    console.log(e)
    alert('Unable to download export')
  })
}

const gameOver = computed(() => game.value?.gameState.tag === "IsOver")

provide('chooseDeck', chooseDeck)
provide('choosePaymentAmounts', choosePaymentAmounts)
provide('chooseAmounts', chooseAmounts)
provide('switchInvestigator', switchInvestigator)
provide('solo', solo)
</script>

<template>
  <div id="game" v-if="ready && game && playerId">
    <CardOverlay />
    <div v-if="socketError" class="socketWarning">
       <p>Your game is out of sync, trying to reconnect...</p>
    </div>
    <div v-if="game.gameState.tag === 'IsPending'" class="invite-container">
      <header>
        <h2>Waiting for more players</h2>
      </header>
      <div id='invite'>
        <div v-if="playerId == game.activePlayerId">
          <p>Invite them with this url:</p>
          <div class="invite-link">
            <input type="text" :value="source"><button @click="copy()"><font-awesome-icon icon="copy" /></button>
          </div>
        </div>
      </div>
    </div>
    <template v-else>
      <div class="game-main">
        <div v-if="gameCard" class="revelation">
          <div class="revelation-container">
            <h2>{{gameCard.title}}</h2>
            <div class="revelation-card-container">
              <div class="revelation-card">
                <CardView :game="game" :card="gameCard.card" :playerId="playerId" />
                <img v-if="gameCard.card.tag === 'PlayerCard'" :src="imgsrc('player_back.jpg')" class="card back" />
                <img v-else :src="imgsrc('back.png')" class="card back" />
              </div>
              <button @click="continueUI">OK</button>
            </div>
          </div>
        </div>
        <div v-if="tarotCards.length > 0" class="revelation">
          <div class="revelation-container">
            <div class="revelation-card-container">
              <div class="tarot-cards">
                <div
                    v-for="(tarotCard, idx) in tarotCards"
                    :key="idx"
                    class="tarot-card"
                  >
                  <div class="card-container">
                    <img :src="imgsrc(`tarot/${tarotCardImage(tarotCard)}`)" class="tarot" :class="tarotCard.facing" />
                  </div>
                  <img :src="imgsrc('tarot/back.jpg')" class="card back" />
                </div>
              </div>
              <button @click="continueUI">OK</button>
            </div>
          </div>
        </div>
        <CampaignSettings
          v-if="game.campaign && !gameOver && question && question.tag === 'PickCampaignSettings'"
          :game="game"
          :campaign="game.campaign"
          :playerId="playerId"
        />
        <Campaign
          v-else-if="game.campaign"
          :game="game"
          :gameLog="gameLog"
          :playerId="playerId"
          @choose="choose"
          @update="update"
        />
        <ScenarioSettings
          v-else-if="game.scenario && !gameOver && question && question.tag === 'PickScenarioSettings'"
          :game="game"
          :scenario="game.scenario"
          :playerId="playerId"
        />
        <StandaloneScenario
          v-else-if="game.scenario && !gameOver"
          :game="game"
          :playerId="playerId"
          @choose="choose"
          @update="update"
        />
        <div class="sidebar" v-if="game.scenario && game.gameState.tag === 'IsActive' || game.gameState.tag === 'IsOver'">
          <GameLog :game="game" :gameLog="gameLog" @undo="undo" />
          <router-link class="button-link" :to="`/games/${game.id}/log`" v-slot="{href, navigate}"
  >
            <button :href="href" @click="navigate">View Log</button>
          </router-link>
          <button @click="debug.toggle">Toggle Debug</button>
          <button @click="debugExport">Debug Export</button>
        </div>
        <div class="game-over" v-if="gameOver">
          <p>Game over</p>
          <CampaignLog v-if="game !== null" :game="game" :cards="cards" />
        </div>
        <div v-if="!game.scenario">
          <GameLog :game="game" :gameLog="gameLog" @undo="undo" />
        </div>
      </div>
    </template>
  </div>
</template>

<style lang="scss" scoped>
.action { border: 5px solid $select; border-radius: 15px; }

#game {
  width: 100vw;
  height: calc(100vh - 40px);
  display: flex;
  flex-direction: column;
  flex: 1;
  position: relative;
}

.game-main {
  width: 100vw;
  height: calc(100vh - 80px);
  display: flex;
  flex: 1;
}

.socketWarning  {
  backdrop-filter: blur(3px);
  background-color: rgba(0,0,0,0.8);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  display: flex;
  z-index: 100;

  justify-content: center;
  align-items: center;
  justify-self: center;
  align-self: center;

  p {
    padding: 10px;
    background: #FFF;
    border-radius: 4px;
  }
}

.sidebar {
  height: 100%;
  width: 25vw;
  max-width: 500px;
  display: flex;
  flex-direction: column;
  background: #d0d9dc;

  @media (prefers-color-scheme: dark) {
    background: #1C1C1C;
  }

  box-sizing: border-box;
}

#invite {
  background-color: #15192C;
  color: white;
  padding: 20px;
  width: 800px;
  margin: 0 auto;
  margin-top: 20px;
  border-radius: 5px;
  text-align: center;
  p { margin: 0; padding: 0; margin-bottom: 20px; font-size: 1.3em; }
}

.invite-container {
  margin-top: 50px;
  h2 {
    color: #656A84;
    margin-left: 10px;
    text-transform: uppercase;
    padding: 0;
    margin: 0;
  }
}

header {
  display: flex;
  flex-direction: column;
}

.invite-link {
  input {
    color: #26283B;
    font-size: 1.3em;
    width: 60%;
    border-right: 0;
    border-radius: 3px 0 0 3px;
    padding: 5px;
  }
  button {
    font-size: 1.3em;
    border-radius: 0 3px 3px 0;
    padding: 5px 10px;
    position: relative;

    &:before {
        content: '';
        display: none;
        position: absolute;
        z-index: 9998;
        top: 35px;
        left: 15px;
        width: 0;
        height: 0;

        border-left: 5px solid transparent;
        border-right: 5px solid transparent;
        border-bottom: 5px solid rgba(0,0,0,.72);
    }

    &:after {
      content: 'Copied!';
      display: none;
      position: absolute;
      z-index: 9999;
      top: 40px;
      left: -37px;
      width: 114px;
      height: 36px;

      color: #fff;
      font-size: 10px;
      line-height: 36px;
      text-align: center;

      background: rgba(0,0,0,.72);
      border-radius: 3px;
    }

    &:active, &:focus {
      outline: none;

      &:hover {
        background-color: #eee;

        &:before, &:after {
          display: block;
        }
      }
    }
  }
}

.button-link {
  display: block;
  width: 100%;
  text-decoration: none;
  button {
    display: block;
    width: 100%;
  }
}

header {
  font-family: Teutonic;
  font-size: 2em;
  text-align: center;
}

.game-over {
  flex-grow: 1;
  display: flex;
  flex-direction: column;
  align-items: center;

  p {
    text-transform: uppercase;
    background: rgba(0, 0, 0, 0.5);
    width: 80%;
    padding: 10px 20px;
    color: white;
    text-align: center;
  }
}

@keyframes revelation {
  0% {
    opacity: 0;
    transform: scale(0);
  }

  65% {
    transform: scale(1.3);
  }

  100% {
    opacity: 1;
    transform: scale(1);
  }
}

@keyframes anim {
	0%,
	100% {
		border-radius: 30% 70% 70% 30% / 30% 52% 48% 70%;
		//box-shadow: 10px -2vmin 4vmin LightPink inset, 10px -4vmin 4vmin MediumPurple inset, 10px -2vmin 7vmin purple inset;
	}

	10% {
		border-radius: 50% 50% 20% 80% / 25% 80% 20% 75%;
	}

	20% {
		border-radius: 67% 33% 47% 53% / 37% 20% 80% 63%;
	}

	30% {
		border-radius: 39% 61% 47% 53% / 37% 40% 60% 63%;
		//box-shadow: 20px -4vmin 8vmin hotpink inset, -1vmin -2vmin 6vmin LightPink inset, -1vmin -2vmin 4vmin MediumPurple inset, 1vmin 4vmin 8vmin purple inset;
	}

	40% {
		border-radius: 39% 61% 82% 18% / 74% 40% 60% 26%;
	}

	50% {
		border-radius: 100%;
		//box-shadow: 40px 4vmin 16vmin hotpink inset, 40px 2vmin 5vmin LightPink inset, 40px 4vmin 4vmin MediumPurple inset, 40px 6vmin 8vmin purple inset;
	}

	60% {
		border-radius: 50% 50% 53% 47% / 72% 69% 31% 28%;
	}

	70% {
		border-radius: 50% 50% 53% 47% / 26% 22% 78% 74%;
		//box-shadow: 1vmin 1vmin 8vmin LightPink inset, 2vmin -1vmin 4vmin MediumPurple inset, -1vmin -1vmin 16vmin purple inset;
	}

	80% {
		border-radius: 50% 50% 53% 47% / 26% 69% 31% 74%;
	}

	90% {
		border-radius: 20% 80% 20% 80% / 20% 80% 20% 80%;
	}
}

@property --gradient-angle {
  syntax: "<angle>";
  initial-value: 0deg;
  inherits: false;
}

@keyframes rotation {
  0% { --gradient-angle: 360deg; }
  100% { --gradient-angle: 0deg; }
}

@keyframes glow {
  0% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
      drop-shadow(0 0 7vmin MediumOrchid);
  }
  50% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
      drop-shadow(0 0 7vmin Black);
  }
  100% {
    filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
      drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
      drop-shadow(0 0 7vmin MediumOrchid);
  }
}

.revelation {
  //--clr-1: Indigo;
  //--clr-2: MediumOrchid;
  //--clr-3: DarkOrchid;
  //background: conic-gradient(
  //  from var(--gradient-angle),
  //  var(--clr-1),
  //  var(--clr-2),
  //  var(--clr-3),
  //  var(--clr-2),
  //  var(--clr-1));
  padding: 30px;
  position: absolute;
  //background: Black;
  transform: all 0.5s;
  z-index: 1000;
  display: flex;
  flex-direction: column;
  color: white;
  text-align:center;
  margin: auto;
  inset: 0;
  width: fit-content;
  height: fit-content;
  animation: revelation 0.3s ease-in-out, glow 4s cubic-bezier(0.550, 0.085, 0.680, 0.530) infinite;

  //animation: revelation 0.3s ease-in-out, anim 30s infinite, rotation 30s linear infinite;

  display: grid;
  // glow effect
  filter: drop-shadow(0 0 3vmin Indigo) drop-shadow(0 5vmin 4vmin Orchid)
		drop-shadow(2vmin -2vmin 15vmin MediumSlateBlue)
		drop-shadow(0 0 7vmin MediumOrchid);

  button {
    width: 100%;
    border: 0;
    padding: 10px;
    text-transform: uppercase;
    background-color: #532e61;
    font-weight: bold;
    color: #EEE;
    font: Arial, sans-serif;
    &:hover {
      background-color: #311b3e;
    }

  i {
    font-style: normal;
  }
}


  h2 {
    font-family: Teutonic;
    text-transform: uppercase;
    margin: 0;
    padding: 0;
    font-size: 2.5em;
  }

  :deep(.card) {
    animation: revelation 0.6s ease-in-out;
    width: 300px !important;
    overflow: hidden;
  }
}

.revelation-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  align-self: center;
  align-content: center;
  justify-content: center;
  justify-items: center;
  justify-self: center;
}

@keyframes flip-back {
  0% {
    opacity: 1;
    transform: rotateY(0deg);
  }

  49% {
    opacity: 1;
  }

  50% {
    opacity: 0;
  }

  100% {
    transform: rotateY(-180deg);
    opacity: 0;
  }
}

@keyframes flip-front {
  0% {
    transform: rotateY(180deg);
    opacity: 0;
  }

  49% {
    opacity: 0;
  }

  50% {
    opacity: 1;
  }

  100% {
    opacity: 1;
    transform: rotateY(0deg);
  }

}


.revelation-card-container {
  display: flex;
  flex-direction: column;
  width: fit-content;
  height: fit-content;

  .tarot-cards {
    gap: 15px;
    display: flex;
    flex-direction: row;
    width: fit-content;
    height: fit-content;
  }

  .revelation-card {
    position: relative;
    width: 300px;
    padding-bottom: 15px;
    aspect-ratio: 5/7;
    perspective: 1000px;
    &:nth-child(1) {
      animation-delay: 0.3s;
    }

    &:nth-child(2) {
      animation-delay: 0.6s;
    }

    &:nth-child(3) {
      animation-delay: 0.9s;
    }

    :deep(.card-container) {
      transform: rotateY(-180deg);
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-front 0.3s linear;
      //animation-delay: 0.3s;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }

    .card-container {
      opacity: 0;
      transform-style: preserve-3d;
    }

    .card.back {
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-back 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }
  }

  .tarot {
    width: 300px;
    aspect-ratio: 8/14;
  }

  .tarot-card:nth-child(1) {
    animation-delay: 0.3s;
  }

  .tarot-card:nth-child(2) {
    animation-delay: 0.6s;
  }

  .tarot-card:nth-child(3) {
    animation-delay: 0.9s;
  }

  .tarot-card {
    position: relative;
    width: 300px;
    padding-bottom: 15px;
    aspect-ratio: 8/14;
    perspective: 1000px;
    .Reversed {
      transform: rotateZ(180deg);
    }
    .card-container {
      transform: rotateY(-180deg);
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-front 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }

    img {
      pointer-events: none;
    }

    .card-container {
      opacity: 0;
      transform-style: preserve-3d;
      pointer-events: none;
      img {
        pointer-events: none;
      }
    }

    .card.back {
      transform-style: preserve-3d;
      position: absolute;
      top: 0;
      left: 0;
      backface-visibility: hidden;
      animation: flip-back 0.3s linear;
      animation-fill-mode: forwards;
      animation-delay: inherit;
    }
  }
}
</style>
