<script lang="ts" setup>
import type { Message } from '@/arkham/types/Message';

export interface Props {
  choice: Message
  index: number
}

const emit = defineEmits(['choose'])
const choose = (idx: number) => emit('choose', idx)

// #001721

defineProps<Props>()
</script>
<template>
  <div class="intro-text">
    <button @click="choose(index)">{{choice.contents[0].contents}}</button>
    <div class="entry">
      <h1 v-if="choice.contents[1].contents[0]">{{choice.contents[1].contents[0]}}</h1>
      <p
        v-for="(paragraph, index) in choice.contents[1].contents[1]"
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
