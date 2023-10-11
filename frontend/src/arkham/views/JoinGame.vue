<script lang="ts" setup>
import { useRouter } from 'vue-router'
import { joinGame } from '@/arkham/api'

export interface Props {
  gameId: string
}
const props = defineProps<Props>()

const router = useRouter()

async function join() {
    joinGame(props.gameId)
      .then((game) => router.push(`/games/${game.id}`));
}
</script>

<template>
  <div class="container">
    <div>
      <header>
        <router-link to="/" class="back-link">‹</router-link>
        <h2>Join Game</h2>
      </header>

      <form id="join-game" @submit.prevent="join">
        <button type="submit" :disabled="disabled">Join</button>
      </form>
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
    background: #F2F2F2;
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
</style>
