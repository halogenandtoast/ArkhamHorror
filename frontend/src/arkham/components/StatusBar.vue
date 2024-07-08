<script lang="ts" setup>
import { useI18n } from 'vue-i18n';
import { ref, computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import { replaceIcons } from '@/arkham/helpers';
import { MessageType } from '@/arkham/types/Message';
import { QuestionType } from '@/arkham/types/Question';
import Draggable from '@/components/Draggable.vue';
import Question from '@/arkham/components/Question.vue';

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])

async function choose(idx: number) {
  emit('choose', idx)
}

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const question = computed(() => props.game.question[props.playerId])
const hide = ref(false)


const cardLabels = computed(() =>
  choices.value.
    flatMap((choice, index) => {
      return choice.tag === "CardLabel" ? [{choice, index}] : []
    }))

// focused cards are handled by the player's choice modal
const searchedCards = computed(() => {
  const investigator = Object.values(props.game.investigators).find((i) => i.playerId === props.playerId)
  const playerCards = Object.values(investigator?.foundCards ?? [])
  if (playerCards.length > 0) {
    return playerCards
  }

  const encounterCards = Object.values(props.game.foundCards)
  if (encounterCards.length > 0) {
    return encounterCards
  }

  return []
})

const focusedCards = computed(() => {
  if (searchedCards.value.length > 0) {
    return []
  }

  return props.game.focusedCards
})

const showChoices = computed(() => !props.game.skillTest && searchedCards.value.length == 0 && focusedCards.value.length == 0 && choices.value.some((c) => { return c.tag === MessageType.DONE || c.tag === MessageType.LABEL || c.tag === MessageType.SKILL_LABEL || c.tag === MessageType.SKILL_LABEL_WITH_LABEL || c.tag == MessageType.PORTRAIT_LABEL }))

const title = computed(() => {
  if (props.game.skillTest) {
    return null
  }
  if (focusedCards.value.length > 0) {
    return null
  }

  if (searchedCards.value.length > 0) {
    return null
  }

  if (cardLabels.value.length > 0) {
    return "Choose"
  }

  if (question.value && question.value.tag === 'QuestionLabel') {
    return question.value.label
  }

  if (question.value && question.value.tag === QuestionType.READ) {
    if (question.value.flavorText.title) {
      return question.value.flavorText.title
    }

    return "Story"
  }

  if (question.value && question.value.tag === QuestionType.DROP_DOWN) {
    return "Choose one"
  }

  if (showChoices.value === true) {
    return "Choose"
  }

  if (focusedChaosTokens.value.length > 0) {
    return "Choose"
  }

  return null
})

const { t } = useI18n()

const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

const label = function(body: string) {
  if (body.startsWith("$")) {
    return t(body.slice(1))
  }
  return replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>').replace(/\*([^*]*)\*/g, '<i>$1</i>')
}

</script>

<template>
  <Draggable v-if="title" class="modal" :class="{ hide }">
    <template #handle><h2 v-html="label(title)"></h2></template>
    <section class="status-bar">
      <Question :game="game" :playerId="playerId" @choose="choose" />
    </section>
  </Draggable>
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
  width: 50vw;
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
  width: 100%;
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
</style>
