<script lang="ts" setup>
import { computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import type { Read } from '@/arkham/types/Question';
import Token from '@/arkham/components/Token';

export interface Props {
  game: Game
  question: Read
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)

const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)
</script>
<template>
  <div class="intro-text">
    <div class="entry">
      <h1 v-if="question.flavorText.title">{{question.flavorText.title}}</h1>
      <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :investigatorId="investigatorId" :game="game" @choose="() => {}" />
      <p
        v-for="(paragraph, index) in question.flavorText.body"
        :key="index"
      >{{paragraph}}</p>
    </div>
    <div class="options">
      <button v-for="(readButton, readIndex) in question.readChoices" @click="choose(readIndex)" :key="readIndex"><i class="option"></i>{{readButton.label}}</button>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.entry {
  background: #DCD6D0;
  padding: 20px;
  box-shadow: inset 0 0 170px rgba(0,0,0,0.5), 1px 1px 3px rgba(0,0,0,0.6);
}

.intro-text {
  font-size: 1.5em;
  overflow: auto;
  height: 100%;
  background: #001721;
  padding: 20px;
  box-sizing: border-box;
  h1 {
    font-family: "Teutonic";
    font-weight: 500;
    color: #38615F;
    margin: 0;
    padding-bottom: 2px;
    margin-bottom: 10px;
    border-bottom: 1px solid #38615f;
    &::after {
      display: block;
      content: " ";
      margin-top: 2px;
      border-bottom: 1px solid #38615f;
    }
  }

  p {
    margin: 10px;
    font-style: italic;
  }
}

button {
  width: 100%;
  border: 0;
  text-align: left;
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


.option {
  &:before {
    font-family: "ArkhamIcons";
    content: "\E91A";
    margin-right: 10px;
  }
}
</style>
