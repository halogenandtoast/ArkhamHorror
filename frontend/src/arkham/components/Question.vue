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
const choose = (idx: number) => emit('choose', idx)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const question = computed(() => props.game.question[props.playerId])
const { t } = useI18n()
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

const showChoices = computed(() => !props.game.skillTest && focusedCards.value.length == 0 && choices.value.some((c) => { return c.tag === MessageType.DONE || c.tag === MessageType.LABEL || c.tag === MessageType.SKILL_LABEL || c.tag === MessageType.SKILL_LABEL_WITH_LABEL || c.tag == MessageType.PORTRAIT_LABEL }))

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

      <div class="question-label" v-if="question && question.tag === 'DropDown'">
        <DropDown @choose="choose" :options="question.options" />
      </div>

      <div class="question-label" v-if="question && question.tag === 'QuestionLabel' && question.question.tag === 'DropDown'">
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
    </template>
