<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import { QuestionType } from '@/arkham/types/Question';
import { MessageType } from '@/arkham/types/Message';
import { imgsrc } from '@/arkham/helpers';
import StoryEntry from '@/arkham/components/StoryEntry.vue';
import PickSupplies from '@/arkham/components/PickSupplies.vue';
import * as ArkhamGame from '@/arkham/types/Game';

export interface Props {
  game: Game
  investigatorId: string
}

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))
const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const question = computed(() => props.game.question[props.investigatorId])
const cardLabelImage = (cardCode: string) => {
  return imgsrc(`cards/${cardCode.replace('c', '')}.jpg`);
}

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <template v-if="question && question.tag === QuestionType.READ">
    <StoryEntry
      :game="game"
      :investigatorId="investigatorId"
      :question="question"
      @choose="choose"
    />
  </template>
  <div class="question-label" v-else-if="question && question.tag === 'QuestionLabel'">
    <p>{{question.label}}</p>

    <div class="label-choices">
      <template v-for="(choice, index) in question.question.choices" :key="index">
        <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
          <button @click="choose(index)" v-tooltip="choice.tooltip">{{choice.label}}</button>
        </template>
        <template v-if="choice.tag === MessageType.LABEL">
          <button @click="choose(index)">{{choice.label}}</button>
        </template>
        <template v-if="choice.tag === MessageType.CARD_LABEL">
          <a href='#' @click.prevent="choose(index)">
            <img class="card" :src="cardLabelImage(choice.cardCode)"/>
          </a>
        </template>
      </template>
    </div>
  </div>

  <div class="question-label" v-else-if="question && question.tag === 'PickSupplies'">
    <PickSupplies :question="question" @choose="choose" />
  </div>
  <template v-else-if="choices.length > 0">
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

.card {
  border-radius: 15px;
}

.label-choices {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}
</style>
