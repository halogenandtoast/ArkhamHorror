<script lang="ts" setup>
import { useWebSocket, useClipboard } from '@vueuse/core'
import { withDefaults, ref, provide, onUnmounted, watch } from 'vue'
import { useRoute } from 'vue-router'
import * as Arkham from '@/arkham/types/Game'
import { fetchGame, updateGame, updateGameAmounts, updateGamePaymentAmounts, updateGameRaw } from '@/arkham/api'
import GameLog from '@/arkham/components/GameLog.vue'
import api from '@/api';
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import Scenario from '@/arkham/components/Scenario.vue'
import Campaign from '@/arkham/components/Campaign.vue'
import { onBeforeRouteLeave } from 'vue-router'

export interface Props {
  gameId: string
  spectate?: boolean
}

const props = withDefaults(defineProps<Props>(), { spectate: false })

const debug = ref(false)
provide('debug', debug)
const ready = ref(false)
const solo = ref(false)
const socketError = ref(false)
const game = ref<Arkham.Game | null>(null)
const investigatorId = ref<string | null>(null)
const gameLog = ref<readonly string[]>(Object.freeze([]))

const route = useRoute()
const spectate = route.fullPath.endsWith('/spectate')

const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
const spectatePrefix = spectate ? "/spectate" : ""
const websocketUrl = `${baseURL}/api/v1/arkham/games/${props.gameId}${spectatePrefix}`.replace(/https/, 'wss').replace(/http/, 'ws')

const onError = () => socketError.value = true
const onConnected = () => socketError.value = false

const { data, close } = useWebSocket(websocketUrl, { autoReconnect: true, onError, onConnected })

watch(data, (newData) => {
  const msg = JSON.parse(newData)

  if (msg.tag === "GameMessage") {
    gameLog.value = Object.freeze([...gameLog.value, msg.contents])
  }

  if (msg.tag === "GameUpdate") {
    Arkham.gameDecoder.decodeToPromise(msg.contents)
      .then((updatedGame) => {
        game.value = updatedGame;
        if (solo.value) {
          if (Object.keys(game.value.question).length == 1) {
            investigatorId.value = Object.keys(game.value.question)[0]
          } else if (game.value.activeInvestigatorId !== investigatorId.value) {
            investigatorId.value = Object.keys(game.value.question)[0]
          }
        }

      });
  }
}, [data])

fetchGame(props.gameId, spectate).then(({ game: newGame, investigatorId: newInvestigatorId, multiplayerMode}) => {
  game.value = newGame;
  solo.value = multiplayerMode === "Solo";
  gameLog.value = Object.freeze(newGame.log);
  investigatorId.value = newInvestigatorId;
  ready.value = true;
});

async function choose(idx: number) {
  if (idx !== -1 && game.value && !spectate) {
    updateGame(props.gameId, idx, investigatorId.value);
  }
}

async function choosePaymentAmounts(amounts: Record<string, number>): Promise<void> {
  if(game.value && !spectate) {
    updateGamePaymentAmounts(props.gameId, amounts)
  }
}

provide('choosePaymentAmounts', choosePaymentAmounts)

async function chooseAmounts(amounts: Record<string, number>): Promise<void> {
  if(game.value && !spectate) {
    updateGameAmounts(props.gameId, amounts)
  }
}

provide('chooseAmounts', chooseAmounts)

/* eslint-disable @typescript-eslint/no-explicit-any */
const debugChoose = async (message: any) => updateGameRaw(props.gameId, message)
provide('debugChoose', debugChoose)

const switchInvestigator = (newInvestigatorId: string) => investigatorId.value = newInvestigatorId
provide('switchInvestigator', switchInvestigator)
provide('solo', solo)

async function update(state: Arkham.Game) {
  game.value = state;
}

onBeforeRouteLeave(() => close ())
onUnmounted(() => close())

const toggleDebug = () => debug.value = !debug.value

const debugExport = () => {
  fetch(new Request(`${api.defaults.baseURL}/arkham/games/${props.gameId}/export`))
  .then(resp => resp.blob())
  .then(blob => {
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.style.display = 'none';
    a.href = url;
    // the filename you want
    a.download = 'arkham-debug.json';
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
  })
  .catch((e) => {
    console.log(e)
    alert('Unable to download export')
  });
}

const source = ref(`${window.location.href}/join`) // fix-syntax`
const { copy } = useClipboard({ source })
</script>

<template>
  <div id="game" v-if="ready">
    <div v-if="socketError" class="socketWarning">
       <p>Your game is out of sync, trying to reconnect...</p>
    </div>
    <div v-if="game.gameState === 'IsPending'" class="invite-container">
      <header>
        <h2>Waiting for more players</h2>
      </header>
      <div id='invite'>
        <div v-if="investigatorId == game.leadInvestigatorId">
          <p>Invite them with this url:</p>
          <div class="invite-link">
            <input type="text" :value="source"><button @click="copy()"><font-awesome-icon icon="copy" /></button>
          </div>
        </div>
      </div>
    </div>
    <template v-else>
      <div class="game-main">
        <Campaign
          v-if="game.campaign"
          :game="game"
          :gameLog="gameLog"
          :investigatorId="investigatorId"
          @choose="choose"
          @update="update"
        />
        <Scenario
          v-else-if="game.scenario && !game.gameOver"
          :game="game"
          :gameLog="gameLog"
          :scenario="game.scenario"
          :investigatorId="investigatorId"
          @choose="choose"
          @update="update"
        />
        <div class="sidebar" v-if="game.scenario && game.gameState !== 'IsPending'">
          <CardOverlay />
          <GameLog :game="game" :gameLog="gameLog" />
          <router-link class="button-link" :to="`/games/${game.id}/log`" v-slot="{href, navigate}"
  >
            <button :href="href" @click="navigate">View Log</button>
          </router-link>
          <button @click="toggleDebug">Toggle Debug</button>
          <button @click="debugExport">Debug Export</button>
        </div>
        <div v-if="game.gameOver">
          <p>Game over</p>

          <div v-for="entry in game.campaign.log.recorded" :key="entry">
            {{entry}}
          </div>

          <div v-for="(entry, idx) in game.campaign.log.recordedSets" :key="idx">
            {{entry[0]}}: {{entry[1].join(", ")}}
          </div>
        </div>
      </div>
    </template>
  </div>
</template>

<style lang="scss" scoped>
.action { border: 5px solid $select; border-radius: 15px; }

#game {
  width: 100vw;
  display: flex;
  flex-direction: column;
  flex: 1;
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
</style>
