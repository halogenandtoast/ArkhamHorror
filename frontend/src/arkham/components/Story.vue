<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import { QuestionType } from '@/arkham/types/Question';
import StoryEntry from '@/arkham/components/StoryEntry.vue';
import * as ArkhamGame from '@/arkham/types/Game';

export interface Props {
  game: Game
  investigatorId: string
}

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const question = computed(() => props.game.question[props.investigatorId])

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <template v-if="question.tag === QuestionType.READ">
    <StoryEntry
      :question="question"
      @choose="choose"
    />
  </template>

  <template v-if="choices.length > 0">
    <div class="choices">
      <template v-for="(choice, index) in choices" :key="index">
        <div v-if="choice.tag === 'Done'">
          <button @click="choose(index)">{{choice.label}}</button>
        </div>
        <div v-if="choice.tag === 'Label'">
          <button @click="choose(index)">{{choice.label}}</button>
        </div>
      </template>
    </div>
  </template>

  <div class="question-label" v-if="question && question.tag === 'QuestionLabel'">
    <p>{{question.contents[0]}}</p>

    <div class="label-choices">
      <template v-for="(choice, index) in question.contents[1].contents" :key="index">
        <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
          <button @click="choose(index)" v-tooltip="choice.contents[1]">{{choice.contents[0]}}</button>
        </template>
      </template>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.question-label {
  display: flex;
  flex-direction: column;
  align-items: center;
  height: 100vh;
  background: #26283B;
}

p {
  color: #666;
  font-size: 2em;
}

button {
  border: 0;
  margin: 0 10px;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
  }
}
</style>
