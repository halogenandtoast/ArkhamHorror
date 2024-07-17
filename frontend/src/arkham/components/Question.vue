<script lang="ts" setup>
import { useI18n } from 'vue-i18n';
import { MessageType } from '@/arkham/types/Message';
import { computed } from 'vue';
import { imgsrc, replaceIcons } from '@/arkham/helpers';
import { QuestionType } from '@/arkham/types/Question';
import * as ArkhamGame from '@/arkham/types/Game';
import { tarotCardImage } from '@/arkham/types/TarotCard';
import DropDown from '@/components/DropDown.vue';
import Token from '@/arkham/components/Token.vue';
import type { Game } from '@/arkham/types/Game';

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const { t } = useI18n()
const choose = (idx: number) => emit('choose', idx)

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const question = computed(() => props.game.question[props.playerId])
const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

// focused cards are handled by the player's choice modal
const focusedCards = computed(() => {
  const investigator = Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
  const playerCards = Object.values(investigator?.foundCards ?? []).flat()
  if (playerCards.length > 0) {
    return playerCards
  }

  const encounterCards = Object.values(props.game.foundCards).flat()
  if (encounterCards.length > 0) {
    return encounterCards
  }

  return props.game.focusedCards
})

const showChoices = computed(() => !props.game.skillTest && choices.value.some((c) => { return c.tag === MessageType.DONE || c.tag === MessageType.LABEL || c.tag === MessageType.SKILL_LABEL || c.tag === MessageType.SKILL_LABEL_WITH_LABEL || c.tag == MessageType.PORTRAIT_LABEL }))

const label = function(body: string) {
  if (body.startsWith("$")) {
    return t(body.slice(1))
  }
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
}

const cardLabelImage = (cardCode: string) => {
  return imgsrc(`cards/${cardCode.replace('c', '')}.jpg`);
}

const portraitLabelImage = (investigatorId: string) => {
  const player = props.game.investigators[investigatorId]

  if (player.isYithian) {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
}

const tarotLabels = computed(() =>
  choices.value.
    flatMap((choice, index) => {
      return choice.tag === "TarotLabel" ? [{choice, index}] : []
    }))

const cardLabels = computed(() =>
  choices.value.
    flatMap((choice, index) => {
      return choice.tag === "CardLabel" ? [{choice, index}] : []
    }))
</script>

<template>
  <div v-if="cardLabels.length > 0" class="cardLabels">
    <template v-for="{choice, index} in cardLabels" :key="index">
      <img class="card" :src="cardLabelImage(choice.cardCode)" @click="choose(index)" />
    </template>
  </div>

  <div v-if="tarotLabels.length > 0">
    <template v-for="{choice, index} in tarotLabels" :key="index">
      <a href='#' @click.prevent="choose(index)">
        <img class="card" :src="imgsrc(`tarot/${tarotCardImage(choice.tarotCard)}`)"/>
      </a>
    </template>
  </div>

  <div class="intro-text" v-if="question && question.tag === QuestionType.READ">
    <p
      v-for="(paragraph, index) in question.flavorText.body"
      :key="index" v-html="label(paragraph)">
    </p>
  </div>

  <div class="question-label dropdown" v-if="question && question.tag === 'DropDown'">
    <DropDown @choose="choose" :options="question.options" />
  </div>

  <div class="question-label dropdown" v-if="question && question.tag === 'QuestionLabel' && question.question.tag === 'DropDown'">
    <DropDown @choose="choose" :options="question.question.options" />
  </div>

  <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="choose" />

  <div v-if="showChoices" class="choices">
    <template v-for="(choice, index) in choices" :key="index">
      <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
        <button @click="choose(index)" v-tooltip="choice.tooltip">{{choice.label}}</button>
      </template>
      <template v-if="choice.tag === 'PortraitLabel'">
        <img class="portrait card active" :src="portraitLabelImage(choice.investigatorId)" @click="choose(index)" />
      </template>
      <button v-if="choice.tag === MessageType.DONE" @click="choose(index)">{{label(choice.label)}}</button>
      <div v-if="choice.tag === MessageType.LABEL" class="message-label">
        <button v-if="choice.label == 'Choose {skull}'" @click="choose(index)">
          Choose <i class="iconSkull"></i>
        </button>
        <button v-else-if="choice.label == 'Choose {cultist}'" @click="choose(index)">
          Choose <i class="iconCultist"></i>
        </button>
        <button v-else-if="choice.label == 'Choose {tablet}'" @click="choose(index)">
          Choose <i class="iconTablet"></i>
        </button>
        <button v-else-if="choice.label == 'Choose {elderThing}'" @click="choose(index)">
          Choose <i class="iconElderThing"></i>
        </button>
        <button v-else @click="choose(index)" v-html="label(choice.label)"></button>
      </div>

      <a
        v-if="choice.tag === MessageType.SKILL_LABEL"
        class="button"
        @click="choose(index)"
      >
        Use <i :class="`icon${choice.skillType}`"></i>
      </a>

      <a
        v-if="choice.tag === MessageType.SKILL_LABEL_WITH_LABEL"
        class="button"
        @click="choose(index)"
      >
        Use <i :class="`icon${choice.skillType}`">: {{choice.label}}</i>
      </a>

    </template>
  </div>
  <div v-else-if="question && question.tag === 'QuestionLabel'" class="standalone-label">
      {{label(question.label)}}
  </div>
</template>

<style scoped lang="scss">
i {
  font-family: 'Arkham';
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;
}

i.iconSkull {
  &:before {
    font-family: "Arkham";
    content: "\004E";
  }
}

i.iconCultist {
  &:before {
    font-family: "Arkham";
    content: "\0042";
  }
}

i.iconTablet {
  &:before {
    font-family: "Arkham";
    content: "\0056";
  }
}

i.iconElderThing {
  &:before {
    font-family: "Arkham";
    content: "\0043";
  }
}

i.iconSkillWillpower {
  &:before {
    font-family: "Arkham";
    content: "\0041";
  }
}

i.iconSkillIntellect {
  &:before {
    font-family: "Arkham";
    content: "\0046";
  }
}

i.iconSkillCombat {
  &:before {
    font-family: "Arkham";
    content: "\0044";
  }
}

i.iconSkillAgility {
  &:before {
    font-family: "Arkham";
    content: "\0053";
  }
}

section {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: center;
}

.button {
  display: inline-block;
  padding: 5px 10px;
  margin: 2px;
  background-color: #333;
  color: white;
  border: 1px solid #666;
  cursor: pointer;

  &:hover {
    background-color: #111;
  }

  &:active {
    background-color: #666;
    border-color: #111;
  }
}

.intro-text {
  max-width: 50vw;
  text-align: justify;
  background: linear-gradient(#DFDAD8, darken(#DFDAD8, 10%));
  padding: 10px;
  margin: 10px;
  margin-bottom: 0;
  border-radius: 5px;
  box-sizing: border-box;
  font-size: 1.1em;
  -moz-osx-font-smoothing: grayscale;
  -webkit-font-smoothing: antialiased !important;
  -moz-font-smoothing: antialiased !important;
  text-rendering: optimizelegibility !important;
  letter-spacing: .03em;
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
  }
}

.status-bar {
  text-align: center;
}

button {
  transition: all 0.3s ease-in;
  border: 0;
  padding: 10px;
  background-color: #532e61;
  text-align: justify;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
  &:hover {
    background-color: #311b3e;
  }
}

.card {
  width: $card-width;
  margin: 2px;
}

.question-label {
  display: flex;
  flex-direction: column;
  width: 75%;

  p {
    font-size: 2em;
  }
}

.label-choices {
  display: flex;
  flex-direction: row;
  align-self: center;
  button {
    margin-left: 10px;
  }
}

.portrait {
  border-radius: 3px;
  width: $card-width;
  margin-right: 2px;

  &.active {
    border: 1px solid $select;
    cursor:pointer;
  }
}

.status-bar:empty {
  display: none;
}

.hide {
  opacity: 0;
}

.modal {
  transition: opacity 0.3s linear;
}

.choices {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin-inline: 10px;
  button {
    font-size: 1.2em;
    width: 100%;
    white-space: nowrap;
    &:before {
      font-family: "ArkhamIcons";
      content: "\E91A";
      margin-right: 10px;
    }
  }

  &:has(.portrait) {
    flex-direction: row;
    padding: 10px;
    justify-content: center;
  }
}

p {
  font-family: "ArkhamFlavor";
  :deep(i) {
    font-family: "ArkhamCursive";
    text-align: center;
    display: block;
    font-size: 1.3em;
    line-height: 0.3em;
    &:last-child {
      padding-bottom: 1.3em;
    }
  }
}

.message-label {
  margin-top: 10px;
  margin-bottom: 10px;
}

h2 {
  font-family: "Teutonic";
  letter-spacing: 1px;
  font-size: 1.7em;
}

.standalone-label {
  text-transform: uppercase;
  color: white;
  background-color: #222;
}

.dropdown {
  padding: 10px;
  :deep(form) {
    min-width: 30vw;
    display: flex;
    flex-direction: column;
    gap: 10px;

    select {
      font-size: 1.2em;
      padding: 5px;
    }

    button {
      text-align: center;
      font-size: 1.2em;
      transition: all 0.3s ease-in;
      border: 0;
      padding: 10px;
      background-color: #532e61;
      border-radius: 0.6em;
      color: #EEE;
      font: Arial, sans-serif;
      &:hover {
        background-color: #311b3e;
      }
    }
  }
}
</style>
