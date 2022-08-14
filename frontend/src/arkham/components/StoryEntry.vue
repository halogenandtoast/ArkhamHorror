<script lang="ts" setup>
import type { Question } from '@/arkham/types/Question';

export interface Props {
  question: Question
}

const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)

// #001721

defineProps<Props>()
</script>
<template>
  <div class="intro-text">
    <button v-for="(readButton, readIndex) in question.contents[1]" @click="choose(readIndex)" :key="readIndex">{{readButton[0]}}</button>
    <div class="entry">
      <h1 v-if="question.contents[0].title">{{question.contents[0].title}}</h1>
      <p
        v-for="(paragraph, index) in question.contents[0].body"
        :key="index"
      >{{paragraph}}</p>
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
  font-size: 1.8em;
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
  font-size: 1.2em;
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
}
</style>
