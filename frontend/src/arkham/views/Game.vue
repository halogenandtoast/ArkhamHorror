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
            <input type="text" :value="inviteLink"><button @click.prevent="copyInviteLink"><font-awesome-icon icon="copy" /></button>
          </div>
        </div>
      </div>
    </div>
    <div v-else class="game">
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
        :investigatorId="investigatorId"
        @choose="choose"
        @update="update"
      />
      <div class="sidebar" v-if="game.gameState !== 'IsPending'">
        <CardOverlay />
        <GameLog :game="game" :gameLog="gameLog" />
        <button @click="toggleDebug">Toggle Debug</button>
      </div>
      <div v-if="game.gameOver">
        <p>Game over</p>

        <div v-for="entry in game.campaign.contents.log.recorded" :key="entry">
          {{entry}}
        </div>

        <div v-for="(entry, idx) in game.campaign.contents.log.recordedSets" :key="idx">
          {{entry[0]}}: {{entry[1].join(", ")}}
        </div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, ref, provide, onUnmounted } from 'vue'
import * as Arkham from '@/arkham/types/Game'
import { fetchGame, updateGame, updateGameRaw } from '@/arkham/api'
import GameLog from '@/arkham/components/GameLog.vue'
import CardOverlay from '@/arkham/components/CardOverlay.vue';
import Scenario from '@/arkham/components/Scenario.vue'
import Campaign from '@/arkham/components/Campaign.vue'
import { onBeforeRouteLeave } from 'vue-router'

export default defineComponent({
  components: { Scenario, Campaign, GameLog, CardOverlay },
  props: { gameId: { type: String, required: true } },
  setup(props) {
    const debug = ref(false)
    provide('debug', debug)
    const ready = ref(false)
    const solo = ref(false)
    const socketError = ref(false)
    const socket = ref<WebSocket | null>(null)
    const game = ref<Arkham.Game | null>(null)
    const investigatorId = ref<string | null>(null)
    const gameLog = ref<readonly string[]>(Object.freeze([]))

    function connect() {
      const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`;
      socket.value = new WebSocket(`${baseURL}/api/v1/arkham/games/${props.gameId}`.replace(/https/, 'wss').replace(/http/, 'ws'));
      socket.value.addEventListener('open', () => {
        ready.value = true;
        socketError.value = false;
      });
      socket.value.addEventListener('message', (event: MessageEvent) => {
        const data = JSON.parse(event.data)

        if (data.tag === "GameMessage") {
          gameLog.value = Object.freeze([...gameLog.value, data.contents])
        }

        if (data.tag === "GameUpdate") {
          Arkham.gameDecoder.decodePromise(data.contents)
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
      });
      socket.value.addEventListener('error', () => {
        socketError.value = true;
        if (socket.value) {
          socket.value.close();
          socket.value = null;
        }
      });
      socket.value.addEventListener('close', () => {
        socketError.value = true;
        socket.value = null;
        setTimeout(() => connect(), 1000);
      });
    }

    fetchGame(props.gameId).then(({ game: newGame, investigatorId: newInvestigatorId, multiplayerMode}) => {
      game.value = newGame;
      solo.value = multiplayerMode == "Solo";
      gameLog.value = Object.freeze(newGame.log);
      investigatorId.value = newInvestigatorId;
      connect();
    });

    async function choose(idx: number) {
      if (idx !== -1 && game.value) {
        updateGame(props.gameId, idx);
      }
    }

    /* eslint-disable @typescript-eslint/no-explicit-any */
    const debugChoose = async (message: any) => updateGameRaw(props.gameId, message)
    provide('debugChoose', debugChoose)

    const switchInvestigator = (newInvestigatorId: string) => investigatorId.value = newInvestigatorId
    provide('switchInvestigator', switchInvestigator)
    provide('solo', solo)

    async function update(state: Arkham.Game) {
      game.value = state;
    }

    onBeforeRouteLeave(() => { if (socket.value) { socket.value.close(); socket.value = null; } })
    onUnmounted(() => { if (socket.value) { socket.value.close(); socket.value = null; }})

    const toggleDebug = () => debug.value = !debug.value

    const inviteLink = `${window.location.href}/join` // fix-syntax`

    const copyInviteLink = () => {
      navigator.clipboard.writeText(inviteLink);
    }


    return { copyInviteLink, inviteLink, debug, toggleDebug, socketError, ready, game, investigatorId, choose, update, gameLog }
  }
})
</script>

<style lang="scss" scoped>
.action { border: 5px solid #FF00FF; border-radius: 15px; }

.game {
  width: 100vw;
  height: calc(100vh - 40px);
  display: flex;
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
</style>
