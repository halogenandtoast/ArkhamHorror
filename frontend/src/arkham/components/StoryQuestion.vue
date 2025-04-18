<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import { QuestionType } from '@/arkham/types/Question';
import { Done, CardLabel, Label, MessageType, PortraitLabel, TooltipLabel } from '@/arkham/types/Message';
import { imgsrc, formatContent } from '@/arkham/helpers';
import StoryEntry from '@/arkham/components/StoryEntry.vue';
import PickSupplies from '@/arkham/components/PickSupplies.vue';
import ChoiceModal from '@/arkham/components/ChoiceModal.vue';
import * as ArkhamGame from '@/arkham/types/Game';

export interface Props {
  game: Game
  playerId: string
}

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const question = computed(() => props.game.question[props.playerId])
const cardLabelImage = (cardCode: string) => {
  return imgsrc(`cards/${cardCode.replace('c', '')}.avif`);
}

const portraitLabelImage = (investigatorId: string) => {
  const player = props.game.investigators[investigatorId]

  if (player.form.tag === 'YithianForm') {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  if (player.form.tag === 'HomunculusForm') {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
}

const portraitChoices = computed(() => {
  if (!question.value) return

  if (!['QuestionLabel', 'ChooseOne'].includes(question.value.tag)) {
    return []
  }

  if (question.value.tag !== 'ChooseOne' && question.value.question.tag !== 'ChooseOne') {
    return []
  }

  if (question.value.tag === 'ChooseOne') {
    return question.value.choices.flatMap<[PortraitLabel, number]>((c, idx) => c.tag === MessageType.PORTRAIT_LABEL ? [[c, idx]] : [])
  }

  return question.value.question.choices.flatMap<[PortraitLabel, number]>((c, idx) => c.tag === MessageType.PORTRAIT_LABEL ? [[c, idx]] : [])
})

const labelChoices = computed(() => {
  if (!question.value) return

  if (question.value.tag === 'QuestionLabel') {
    if (!['ChooseOne', 'ChooseUpToN', 'ChooseN'].includes(question.value.question.tag)) {
      return []
    }

    return question.value.question.choices.flatMap<[Label | TooltipLabel | CardLabel | Done, number]>((c, idx) => {
      if (c.tag === MessageType.LABEL || c.tag === MessageType.TOOLTIP_LABEL || c.tag === MessageType.CARD_LABEL || c.tag === MessageType.DONE) {
        return [[c, idx]]
      } else {
        return []
      }
    })
  }

  if (['ChooseOne', 'ChooseUpToN', 'ChooseN'].includes(question.value.tag)) {
    return question.value.choices.flatMap<[Label | TooltipLabel | CardLabel | Done, number]>((c, idx) => {
      if (c.tag === MessageType.LABEL || c.tag === MessageType.TOOLTIP_LABEL || c.tag === MessageType.CARD_LABEL || c.tag === MessageType.DONE) {
        return [[c, idx]]
      } else {
        return []
      }
    })
  }
})

const questionImage = computed(() => {

  if (question.value.tag !== 'QuestionLabel') {
    return null
  }

  if(question.value.card) {
    return cardLabelImage(question.value.card)
  }

  return null
})

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <template v-if="question && question.tag === QuestionType.READ">
    <StoryEntry
      :game="game"
      :playerId="playerId"
      :question="question"
      @choose="choose"
    />
  </template>
  <div class="question-label" v-else-if="question && question.tag === 'QuestionLabel'">
    <img :src="questionImage" v-if="questionImage" class="card" />
    <p v-html="formatContent(question.label)"></p>

    <div class="portrait-choices" v-if="portraitChoices.length > 0">
      <template v-for="[choice, index] in portraitChoices" :key="index">
        <template v-if="choice.tag === MessageType.PORTRAIT_LABEL">
          <a href='#' @click.prevent="choose(index)">
            <img class="portrait card active" :src="portraitLabelImage(choice.investigatorId)"/>
          </a>
        </template>
      </template>
    </div>

    <div class="label-choices" v-if="labelChoices.length > 0">
      <div class="card-labels" v-if="labelChoices.some(([choice, _]) => choice.tag === MessageType.CARD_LABEL)">
        <template v-for="[choice, index] in labelChoices" :key="index">
          <template v-if="choice.tag === MessageType.CARD_LABEL">
            <a href='#' @click.prevent="choose(index)">
              <img class="card no-overlay" :src="cardLabelImage(choice.cardCode)"/>
            </a>
          </template>
        </template>
      </div>
      <div class="other-labels" v-for="[choice, index] in labelChoices" :key="index">
        <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
          <button @click="choose(index)" v-tooltip="choice.tooltip">{{choice.label}}</button>
        </template>
        <template v-if="choice.tag === MessageType.LABEL">
          <button @click="choose(index)">{{$t(choice.label)}}</button>
        </template>
        <template v-if="choice.tag === MessageType.DONE">
          <button @click="choose(index)">{{$t(choice.label)}}</button>
        </template>
      </div>
    </div>
  </div>

  <div class="question-label" v-else-if="question && question.tag === 'PickSupplies'">
    <PickSupplies :game="game" :playerId="playerId" :question="question" @choose="choose" />
  </div>
  <template v-else-if="choices.length > 0">
    <div class="choices box">
      <div class="card-labels" v-if="labelChoices.some(([choice, _]) => choice.tag === MessageType.CARD_LABEL)">
        <template v-for="[choice, index] in labelChoices" :key="index">
          <template v-if="choice.tag === MessageType.CARD_LABEL">
            <a href='#' @click.prevent="choose(index)">
              <img class="card no-overlay" :src="cardLabelImage(choice.cardCode)"/>
            </a>
          </template>
        </template>
      </div>
      <template v-for="(choice, index) in choices" :key="index">
        <div v-if="choice.tag === 'Done'">
          <button @click="choose(index)">{{choice.label}}</button>
        </div>
        <div v-if="choice.tag === 'Label'">
          <button @click="choose(index)">{{choice.label}}</button>
        </div>
      </template>
      <div class="portrait-choices" v-if="portraitChoices.length > 0">
        <template v-for="[choice, index] in portraitChoices" :key="index">
          <template v-if="choice.tag === MessageType.PORTRAIT_LABEL">
            <a href='#' @click.prevent="choose(index)">
              <img class="portrait card active" :src="portraitLabelImage(choice.investigatorId)"/>
            </a>
          </template>
        </template>
      </div>
    </div>
  </template>

  <ChoiceModal
    :game="game"
    :playerId="playerId"
    :noStory="true"
    v-else-if="!question || question.tag !== 'PickSupplies'"
    @choose="$emit('choose', $event)"
  />
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
  flex-direction: column;
  gap: 10px;
}

.card-labels {
  display: flex;
  flex-wrap: wrap;
  flex-direction: row;
  gap: 10px;
}

.other-labels {
  display: flex;
  flex-wrap: wrap;
  flex-direction: row;
  gap: 10px;
}

.card {
  width: calc(var(--card-width) * 2);
}
</style>
