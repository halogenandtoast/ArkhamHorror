<script lang="ts" setup>
import type { Game } from '@/arkham/types/Game';
import { ref } from 'vue';
import { useRouter } from 'vue-router'
import { joinGame, fetchJoinGame} from '@/arkham/api'
import GameDetails from '@/arkham/components/GameDetails.vue';

export interface Props {
  gameId: string
}
const props = defineProps<Props>()

const router = useRouter()
const game = ref<Game | null>(null)

fetchJoinGame(props.gameId).then((result: Game) => game.value = result)

async function join() {
    joinGame(props.gameId)
      .then((game) => router.push(`/games/${game.id}`));
}
</script>

<template>
  <div class="container">
    <div>
      <header>
        <h2>Join Game</h2>
      </header>

      <GameDetails v-if="game" :game="game">
        <form id="join-game" @submit.prevent="join">
          <button type="submit">Join</button>
        </form>
      </GameDetails>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.container {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
  margin-top: 10px;
}

#join-game {
  width: 100%;
  color: #FFF;
  background-color: #15192C;
  padding: 10px;
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
  input[type=text] {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
    margin-bottom: 10px;
  }
  select {
    outline: 0;
    border: 1px solid #000;
    padding: 15px;
    background: #F2F2F2;
    width: 100%;
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
</style>
