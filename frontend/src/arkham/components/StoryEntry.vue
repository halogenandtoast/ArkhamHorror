<script lang="ts" setup>
import { computed } from 'vue';
import { Game } from '@/arkham/types/Game';
import type { Read } from '@/arkham/types/Question';
import Token from '@/arkham/components/Token.vue';

export interface Props {
  game: Game
  question: Read
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)

const format = function(body: string) {
  return body.replace(/_([^_]*)_/g, '<b>$1</b>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
}

const readChoices = props.question.readChoices.reduce<{ label: string, index: number}[]>((acc, v, i) => {
  if ("label" in v) {
    return [...acc, { label: v.label, index: i }]
  }
  return acc
}, [] as { label: string, index: number }[])

const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

const formatted = computed(() => {
  return props.question.flavorText.body[0].startsWith("$") ? props.question.flavorText.body[0].slice(1) : null
})

const tformat = (t:string) => t.startsWith("$") ? t.slice(1) : t

</script>
<template>
  <div class="intro-text">
    <div class="entry" v-if="formatted" v-html="$t(formatted)"></div>
    <div v-else class="entry">
      <h1 v-if="question.flavorText.title">{{question.flavorText.title}}</h1>
      <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="() => {}" />
      <p
        v-for="(paragraph, index) in question.flavorText.body"
        :key="index"
        v-html="format(paragraph)"
        ></p>
    </div>
    <div class="options">
      <button
        v-for="readChoice in readChoices"
        @click="choose(readChoice.index)"
        :key="readChoice.index"
      ><i class="option"></i>{{$t(tformat(readChoice.label))}}</button>
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
  color: #333;
  font-size: 1.5em;
  overflow: auto;
  height: 100%;
  background: #001721;
  padding: 20px;
  box-sizing: border-box;
  max-height: 70vh;
  :deep(h1) {
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

  :deep(p) {
    margin: 10px;
    font-style: italic;
  }
}

p {
  font-family: "ArkhamFlavor";
  :deep(i) {
    font-family: "ArkhamCursive";
    display: block;
    font-size: 1.3em;
    line-height: 0.3em;
    &:last-child {
      padding-bottom: 1.3em;
    }
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
