<script lang="ts" setup>
import { useI18n } from 'vue-i18n';
import { handleI18n } from '@/arkham/i18n';
import { choiceRequiresModal, MessageType } from '@/arkham/types/Message';
import { computed, inject, ref, watch, onMounted, h } from 'vue';
import { imgsrc, formatContent } from '@/arkham/helpers';
import { AmountChoice, QuestionType, FlavorTextEntry, FlavorTextModifier } from '@/arkham/types/Question';
import Card from '@/arkham/components/Card.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { tarotCardImage } from '@/arkham/types/TarotCard';
import { cardImage } from '@/arkham/types/Card';
import DropDown from '@/components/DropDown.vue';
import Token from '@/arkham/components/Token.vue';
import type { Game } from '@/arkham/types/Game';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice.vue';
import FormattedEntry from '@/arkham/components/FormattedEntry.vue';

export interface Props {
  game: Game
  playerId: string
  isSkillTest?: boolean
}

const props = withDefaults(defineProps<Props>(), { isSkillTest: false })
const emit = defineEmits(['choose'])
const { t } = useI18n()
const choose = (idx: number) => emit('choose', idx)
const investigator = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId))

function formatEntry(entry: FlavorTextEntry): any {
  console.log(entry)
  switch (entry.tag) {
    //case 'BasicEntry': return h('div', formatContent(entry.text.startsWith('$') ? t(entry.text.slice(1)) : entry.text))
    // case 'I18nEntry': return h('div', formatContent(t(entry.key, entry.variables)))
    // case 'ModifyEntry': return h('div', { class: entryStyles(entry) }, [formatEntry(entry.entry)])
    // case 'CompositeEntry': return h('div', entry.entries.map(formatEntry))
    default: return h('div', "Unknown entry type")
  }
}

function modifierToStyle(modifier: FlavorTextModifier): string {
  switch (modifier) {
    case 'BlueEntry': return 'blue'
    case 'RightAligned': return 'right'
    case 'PlainText': return 'basic'
    case 'InvalidEntry': return 'invalid'
    case 'ValidEntry': return 'valid'
    default: throw new Error("Unknown modifier")
  }
}

function entryStyles(entry: FlavorTextEntry): { [key: string]: boolean } {
  switch (entry.tag) {
    case 'BasicEntry': return {}
    case 'I18nEntry': return {}
    case 'ModifyEntry': return entry.modifiers.map((m) => { return { [modifierToStyle(m)]: true }})
    case 'CompositeEntry': return {}
    default: return {}
  }
}

function zoneToLabel(s: string) {
  switch(s) {
    case "FromDeck": return "From Deck"
    case "FromHand": return "From Hand"
    case "FromDiscard": return "From Discard"
    default: return s
  }
}
const inSkillTest = computed(() => props.game.skillTest !== null)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const choosePaymentAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('choosePaymentAmounts')
const chooseAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('chooseAmounts')
const question = computed(() => props.game.question[props.playerId])
const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)
const searchedCards = computed(() => {
  const playerCards = Object.entries(investigator.value?.foundCards ?? [])

  const playerZones = playerCards.filter(([, c]) => c.length > 0)

  const encounterCards = Object.entries(props.game.foundCards)
  const encounterZones = encounterCards.filter(([, c]) => c.length > 0)

  return [...playerZones, ...encounterZones]
})

const focusedCards = computed(() => {
  if (searchedCards.value.length > 0) {
    return []
  }

  return props.game.focusedCards
})


const showChoices = computed(() => {
  if (props.game.skillTest && !props.isSkillTest) {
    return false
  }
  if (choices.value.some(choiceRequiresModal)) {
    return true
  }
  return props.game.focusedChaosTokens.length > 0 || focusedCards.value.length > 0 || searchedCards.value.length > 0 || paymentAmountsLabel.value || amountsLabel.value
})

const label = function(body: string) {
  if (body.startsWith("$")) {
    return formatContent(handleI18n(body.slice(1), t))
  }
  return formatContent(body)
}

const paymentAmountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return formatContent(question.value.label)
  }

  return null
})

const amountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return formatContent(question.value.label)
  }

  if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.question.label
  }

  return null
})

const paymentAmountsChoices = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.value.paymentAmountChoices
  }

  return []
})

const chooseAmountsChoices = computed<AmountChoice[]>(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.amountChoices
  } else if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question.tag === QuestionType.CHOOSE_AMOUNTS) {
    return question.value.question.amountChoices
  }

  return []
})

const amountSelections = ref<Record<string, number>>({})

const setInitialAmounts = () => {
    const labels = question.value?.tag === QuestionType.CHOOSE_AMOUNTS
      ? question.value.amountChoices.map((choice) => choice.choiceId)
      : (paymentAmountsChoices.value ?? []).map((choice) => choice.choiceId)
    amountSelections.value = labels.reduce<Record<string, number>>((previousValue, currentValue) => {
      previousValue[currentValue] = 0
      return previousValue
    }, {})
  }

const doneLabel = computed(() => {
  const doneIndex = choices.value.findIndex((c) => c.tag === MessageType.DONE)

  if (doneIndex !== -1) {
    return { label: choices.value[doneIndex].label, index: doneIndex } // choices.value[doneIndex].label
  }

  return null
})

onMounted(setInitialAmounts)

watch(
  () => props.game.question[props.playerId],
  setInitialAmounts)

const unmetAmountRequirements = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    const target = question.value.paymentAmountTargetValue
    if (target) {
      switch(target.tag) {
        case 'MaxAmountTarget':
          {
            const maxBound = target.contents
            if (maxBound) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return total > maxBound
            }
            break
          }
        case 'MinAmountTarget':
          {
            const minBound = target.contents
            if (minBound) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return total < minBound
            }
            break
          }
        case 'TotalAmountTarget':
          {
            const requiredTotal = target.contents
            if (requiredTotal) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return total !== requiredTotal
            }
            break
          }
        case 'AmountOneOf':
          {
            const totals = target.contents
            if (totals.length > 0) {
              const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
              return totals.indexOf(total) === -1
            }
            break
          }
      }
    }
    return false
  } else if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    switch(question.value.amountTargetValue.tag) {
      case 'MaxAmountTarget':
        {
          const maxBound = question.value.amountTargetValue.contents
          if (maxBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total > maxBound
          }
          break
        }
      case 'MinAmountTarget':
        {
          const minBound = question.value.amountTargetValue.contents
          if (minBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total < minBound
          }
          break
        }
      case 'TotalAmountTarget':
        {
          const requiredTotal = question.value.amountTargetValue.contents
          if (requiredTotal) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total !== requiredTotal
          }
          break
        }
      case 'AmountOneOf':
        {
          const totals = question.value.amountTargetValue.contents
          if (totals.length > 0) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return totals.indexOf(total) === -1
          }
          break
        }
    }

    return false
  } else if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question.tag === QuestionType.CHOOSE_AMOUNTS) {
    const actual = question.value.question
    switch(actual.amountTargetValue.tag) {
      case 'MaxAmountTarget':
        {
          const maxBound = actual.amountTargetValue.contents
          if (maxBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total > maxBound
          }
          break
        }
      case 'MinAmountTarget':
        {
          const minBound = actual.amountTargetValue.contents
          if (minBound) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total < minBound
          }
          break
        }
      case 'TotalAmountTarget':
        {
          const requiredTotal = actual.amountTargetValue.contents
          if (requiredTotal) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return total !== requiredTotal
          }
          break
        }
      case 'AmountOneOf':
        {
          const totals = actual.amountTargetValue.contents
          if (totals.length > 0) {
            const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)
            return totals.indexOf(total) === -1
          }
          break
        }
    }

    return false
  }

  return true
})

const submitPaymentAmounts = async () => {
  if (choosePaymentAmounts) {
    choosePaymentAmounts(amountSelections.value)
  }
}

const submitAmounts = async () => {
  if (chooseAmounts) {
    chooseAmounts(amountSelections.value)
  }
}

const cardLabelImage = (cardCode: string) => {
  return imgsrc(`cards/${cardCode.replace('c', '')}.avif`);
}

const cardIdImage = (cardId: string) => {
  return (imgsrc(cardImage(props.game.cards[cardId])))
}

const portraitLabelImage = (investigatorId: string) => {
  const player = props.game.investigators[investigatorId]

  if (player.isYithian) {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  return imgsrc(`portraits/${player.cardCode.replace('c', '')}.jpg`)
}

const portraits = computed<[Message, number]>(() =>
  choices.value.map((x: Message, i: number) => [x, i] as [Message, number]).filter(([choice,]) => choice.tag === "PortraitLabel")
)

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

const chaosBagChoice = computed(() => {
  if (props.game.skillTest) {
    //if we are in a skill test, it will handle this display
    return null;
  }
  return props.game.scenario?.chaosBag.choice
})

const cardPiles = computed(() => {
  return choices.value.flatMap((choice, index) => {
    return choice.tag === "CardPile" ? [{pile: choice.pile, index}] : []
  })
})
</script>

<template>
  <ChaosBagChoice v-if="chaosBagChoice" :choice="chaosBagChoice" :game="game" :playerId="playerId" @choose="choose" />
  <div v-if="cardPiles.length > 0" class="cardPiles">
    <div v-for="{pile, index} in cardPiles" :key="pile" class="card-pile" @click="choose(index)">
      <div v-for="card in pile" :key="card" class="pile-card">
        <img class="card" :src="cardIdImage(card.cardId)" />
        <img v-if="card.cardOwner" class="portrait" :src="portraitLabelImage(card.cardOwner)" />
      </div>
    </div>
  </div>

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
    <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
  </div>

  <div class="question-label dropdown" v-if="question && question.tag === 'DropDown'">
    <DropDown @choose="choose" :options="question.options" />
  </div>

  <div class="question-label dropdown" v-if="question && question.tag === 'QuestionLabel' && question.question.tag === 'DropDown'">
    <DropDown @choose="choose" :options="question.question.options" />
  </div>

  <div v-if="!isSkillTest && !inSkillTest && focusedChaosTokens.length > 0" class="tokens">
    <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="choose" />
  </div>

  <div v-if="showChoices" class="choices">
    <div v-if="focusedCards.length > 0 && choices.length > 0" class="modal">
      <div class="modal-contents focused-cards">
        <Card
          v-for="(card, index) in focusedCards"
          :card="card"
          :game="game"
          :playerId="playerId"
          :key="index"
          @choose="$emit('choose', $event)"
        />
      </div>
    </div>
    <div v-if="searchedCards.length > 0 && choices.length > 0" class="modal">
      <div class="modal-contents searched-cards">
        <div v-for="[group, cards] in searchedCards" :key="group" class="group">
          <h2>{{zoneToLabel(group)}}</h2>
          <div class="group-cards">
            <Card
              v-for="card in cards"
              :card="card"
              :game="game"
              :playerId="playerId"
              :key="`${group}-${card}`"
              @choose="$emit('choose', $event)"
            />
          </div>
        </div>
      </div>
    </div>
    <div v-if="paymentAmountsLabel" class="modal amount-modal">
      <div class="modal-contents amount-contents">
        <form @submit.prevent="submitPaymentAmounts" :disabled="unmetAmountRequirements">
          <legend v-html="paymentAmountsLabel"></legend>
          <template v-for="amountChoice in paymentAmountsChoices" :key="amountChoice.investigatorId">
            <div v-if="amountChoice.maxBound !== 0">
              {{amountChoice.title}}
              <input
                type="number"
                :min="amountChoice.minBound"
                :max="amountChoice.maxBound"
                v-model.number="amountSelections[amountChoice.choiceId]"
                onclick="this.select()"
              />
            </div>
          </template>
          <button :disabled="unmetAmountRequirements">Submit</button>
        </form>
      </div>
    </div>
    <div v-if="amountsLabel" class="modal amount-modal">
      <div v-if="searchedCards.length > 0" class="modal-contents searched-cards">
        <div v-for="[group, cards] in searchedCards" :key="group" class="group">
          <h2>{{zoneToLabel(group)}}</h2>
          <div class="group-cards">
            <Card
              v-for="card in cards"
              :card="card"
              :game="game"
              :playerId="playerId"
              :key="`${group}-${card}`"
              @choose="$emit('choose', $event)"
            />
          </div>
        </div>
      </div>
      <div class="modal-contents amount-contents">
        <form @submit.prevent="submitAmounts" :disabled="unmetAmountRequirements">
          <legend v-html="amountsLabel"></legend>
          <template v-for="paymentChoice in chooseAmountsChoices" :key="paymentChoice.choiceId">
            <div v-if="paymentChoice.maxBound !== 0">
              <label :for="`choice-${paymentChoice.choiceId}`" v-html="formatContent(paymentChoice.label)"></label> <input type="number" :min="paymentChoice.minBound" :max="paymentChoice.maxBound" v-model.number="amountSelections[paymentChoice.choiceId]" :name="`choice-${paymentChoice.choiceId}`" onclick="this.select()" />
            </div>
          </template>
          <button :disabled="unmetAmountRequirements">Submit</button>
        </form>
      </div>
    </div>

    <div v-if="portraits.length > 0" class="portraits">
      <template v-for="([choice, index]) in portraits" :key="index">
        <template v-if="choice.tag === 'PortraitLabel'">
          <div class="portrait">
            <img class="active" :src="portraitLabelImage(choice.investigatorId)" @click="choose(index)" />
          </div>
        </template>
      </template>
    </div>

    <template v-for="(choice, index) in choices" :key="index">
      <template v-if="choice.tag === MessageType.TOOLTIP_LABEL">
        <button @click="choose(index)" v-tooltip="choice.tooltip">{{choice.label}}</button>
      </template>
      <template v-if="choice.tag === MessageType.ABILITY_LABEL && choice.ability.type.tag === 'ConstantReaction'">
        <button @click="choose(index)">{{choice.ability.type.label}}</button>
      </template>
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
        Use <i :class="`icon${choice.skillType}`"></i>: {{choice.label}}
      </a>

    </template>
  </div>
  <div v-else-if="question && question.tag === 'QuestionLabel' && question.question.tag !== 'DropDown'" class="standalone-label">
      {{label(question.label)}}
  </div>
  <div v-if="doneLabel">
    <button class="done" @click="$emit('choose', doneLabel.index)" v-html="label(doneLabel.label)"></button>
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

i.iconSkull:before {
  content: "\004E";
}

i.iconCultist:before {
  content: "\0042";
}

i.iconTablet:before {
  content: "\0056";
}

i.iconElderThing:before {
  content: "\0043";
}

i.iconSkillWillpower:before {
  content: "\0041";
}

i.iconSkillIntellect:before {
  content: "\0046";
}

i.iconSkillCombat:before {
  content: "\0044";
}

i.iconSkillAgility:before {
  content: "\0053";
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
}

.button:hover {
  background-color: #111;
}

.button:active {
  background-color: #666;
  border-color: #111;
}

.intro-text {
  color: #222;
  max-width: 50vw;
  text-align: justify;
  background: linear-gradient(#DFDAD8, #c9c4c2);
  padding: 10px;
  margin: 10px;
  border-radius: 5px;
  font-size: 1.1em;
  -moz-osx-font-smoothing: grayscale;
  -webkit-font-smoothing: antialiased !important;
  -moz-font-smoothing: antialiased !important;
  text-rendering: optimizelegibility !important;
  letter-spacing: .03em;
  max-height: 70vh;
  overflow: auto;
  display: flex;
  flex-direction: column;
  gap: 10px;

  &:has(.resolution) {
    background: #BAA898;
  }
  &:has(.checkpoint header h1), &:has(.interlude header h1) {
    padding-top: 2em;
  }
  &:has(.checkpoint), &:has(.interlude) {
    background: #AFA9A9;
    box-shadow: unset;
    overflow: hidden;
    &::after {
      border: 20px solid #D4CCC3;
      border-left-width: 10px;
      border-right-width: 10px;
      position: absolute;
      inset: 0px;
      box-sizing: border-box;
      content: "";
      filter: blur(0.25em);
      z-index: 1;
    }
    h1 {
      color: #19214F;
      border-bottom: 1px solid #19214F;
      &::after {
        border-bottom: 1px solid #19214F;
      }
      font-size: 1.3em;
      font-weight: 500;
    }
    padding: 50px;
    position: relative;
    &::before {
      z-index: 2;
      pointer-events: none;
      position: absolute;
      inset: 10px;
      border-image: url(/img/arkham/checkpoint_fleur.png) 49.9%;
      border-image-repeat: no-repeat;
      border-image-width: 50px;
      content: "";
    }
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
}

button:hover {
  background-color: #311b3e;
}

.card {
  width: min-width(calc(var(--card-width) * 2));
  margin: 2px;
  height: max(calc(var(--card-width) * 2));
}

.question-label {
  display: flex;
  flex-direction: column;
  width: 75%;
}

.question-label p {
  font-size: 2em;
}

.label-choices {
  display: flex;
  flex-direction: row;
  align-self: center;
}

.label-choices button {
  margin-left: 10px;
}

.portrait {
  min-width: fit-content;
  border-radius: 3px;
  margin: 10px;

  img {
    border-radius: 5px;
    width: auto;
    height: max(calc(var(--card-width) * 2));
  }

  img.active {
    border: 1px solid var(--select);
    cursor: pointer;
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
  margin-block: 10px;
  flex-wrap: wrap;
}

.choices button {
  font-size: 1.2em;
  width: 100%;
  white-space: nowrap;
  text-wrap: pretty;
}

.choices button:before {
  font-family: "ArkhamIcons";
  content: "\E91A";
  margin-right: 10px;
}

.choices:has(.portrait) {
  flex-direction: row;
  padding: 10px;
  justify-content: center;
}

p {
  font-family: "ArkhamFlavor";
}

p :deep(i) {
  font-family: "ArkhamCursive";
  text-align: center;
  display: block;
  font-size: 1.3em;
  line-height: 0.3em;
}

p :deep(i):last-child {
  padding-bottom: 1.3em;
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
  padding: 10px;
}

.dropdown {
  padding: 10px;
  background: #735e7b;
  width: 100%;
  border-bottom-left-radius: 15px;
  border-bottom-right-radius: 15px;

  & :deep(button) {
    background: #4a3d50;
    display: inline;
    border: 0;
    color: white;
    padding: 0.5em;
  }

  & :deep(form) {
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
    }

    button:hover {
      background-color: #311b3e;
    }
  }
}

.cardPiles {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.card-pile {
  cursor: pointer;
  display: flex;
  flex-direction: row;
  padding: 10px;
  background: rgba(0, 0, 0, 0.5);
  margin: 10px;
  border-radius: 10px;
  justify-content: center;
  gap: 10px;
}

.card-pile:hover {
  background: rgba(255, 255, 255, 0.2);
}

.card-pile .portrait {
  width: calc(var(--card-width) / 2);
}

.modal-contents {
  display: flex;
  align-items: center;
  padding: 10px;
}

.focused-cards {
  flex-direction: row;
  overflow-x: auto;
  flex-wrap: wrap;
}

.amount-contents {
  background: #735e7b;
  padding: 10px;
  border-bottom-left-radius: 15px;
  border-bottom-right-radius: 15px;
  width: 100%;
  form {
    flex: 1;
  }
}

.amount-contents div {
  display: inline;
}

.amount-contents button {
  background: #4a3d50;
  display: inline;
  border: 0;
  color: white;
  padding: 0.5em;
  margin-left: 0.5em;
}

.amount-contents button[disabled] {
  cursor: not-allowed;
}

.amount-contents input {
  padding: 0.5em;
}

.amount-contents legend {
  font-size: 1.2em;
  font-weight: bold;
}

.amount-contents .selection {
  margin-left: 50px;
}

.amount-contents .selection:nth-of-type(1) {
  margin-left: 0;
}

.choices {
  text-align: center;
}

.choices button {
  transition: all 0.3s ease-in;
  border: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0.6em;
  color: #EEE;
  font: Arial, sans-serif;
}

.choices button:hover {
  background-color: #311b3e;
}

.searched-cards {
  flex-direction: column;
  overflow-x: auto;
}

.group {
  display: flex;
  align-items: center;
  flex-direction: column;
}

.group .group-cards {
  display: flex;
  flex-wrap: wrap;
}

.done {
  width: 100%;
  border: 0;
  margin: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: #532e61;
  font-weight: bold;
  border-radius: 0 0 1.2em 1.2em;
  color: #EEE;
  font: Arial, sans-serif;
}

.done:hover {
  background-color: #311b3e;
}

.tokens {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  padding: 10px;
  gap: 10px;
}

.cardLabels {
  margin: 10px;
  img {
    border: 1px solid var(--select);
    border-radius: 5px;
  }
}

.portraits {
  flex-basis: 100%;
  display: flex;
  flex-direction: row;
  justify-content: center;
  .portrait {
    min-width: fit-content;
  }
}

.intro-text {
  &:has(.resolution) {
    background-color: #BAA597;
    box-shadow: unset;
    overflow: hidden;
    &::after {
      border: 20px solid #D4CCC3;
      border-left-width: 10px;
      border-right-width: 10px;
      position: absolute;
      inset: 0px;
      box-sizing: border-box;
      content: "";
      filter: blur(0.25em);
      z-index: 1;
    }
    h1 {
      color: #19214F;
      border-bottom: 1px solid #19214F;
      &::after {
        border-bottom: 1px solid #19214F;
      }
      font-size: 1.3em;
      font-weight: 500;
    }
    padding: 50px;
    position: relative;
    &::before {
      z-index: 2;
      pointer-events: none;
      position: absolute;
      inset: 10px;
      border-image: url(/img/arkham/resolution_fleur.png) 49.9%;
      border-image-repeat: no-repeat;
      border-image-width: 50px;
      content: "";
    }
  }
}

</style>
