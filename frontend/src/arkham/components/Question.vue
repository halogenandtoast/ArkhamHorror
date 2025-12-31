<script lang="ts" setup>
import { useDbCardStore } from '@/stores/dbCards'
import { chaosTokenImage } from '@/arkham/types/ChaosToken';
import { useI18n } from 'vue-i18n';
import { useDebouncedRef } from '@/composeable/debouncedRef';
import { handleI18n } from '@/arkham/i18n';
import { choiceRequiresModal, MessageType, CardLabel, ChaosTokenLabel } from '@/arkham/types/Message';
import { computed, inject, ref, watch, onMounted } from 'vue';
import { imgsrc, formatContent } from '@/arkham/helpers';
import { AmountChoice, QuestionType } from '@/arkham/types/Question';
import Card from '@/arkham/components/Card.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { tarotCardImage } from '@/arkham/types/TarotCard';
import { cardImage, toCardContents } from '@/arkham/types/Card';
import DropDown from '@/components/DropDown.vue';
import Token from '@/arkham/components/Token.vue';
import type { Game } from '@/arkham/types/Game';
import ExchangeTokens from '@/arkham/components/ExchangeTokens.vue';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice.vue';
import FormattedEntry from '@/arkham/components/FormattedEntry.vue';
import QuestionChoices from '@/arkham/components/QuestionChoices.vue';

export interface Props {
  game: Game
  playerId: string
  isSkillTest?: boolean
}

const grunge = `url(${imgsrc('grunge.png')})`
const black_fleur = `url(${imgsrc('fleur.png')})`
const checkpoint_fleur = `url(${imgsrc('checkpoint_fleur.png')})`
const resolution_fleur = `url(${imgsrc('resolution_fleur.png')})`
const props = withDefaults(defineProps<Props>(), { isSkillTest: false })
const emit = defineEmits(['choose'])
const { t } = useI18n()
const choose = (idx: number) => emit('choose', idx)
const investigator = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId))

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
const questionChoices = computed(() => {
  return choices.value.map((c, idx) => [c, idx]).filter(([choice, _]) => {
    const { tag } = choice
    if (tag === 'AbilityLabel' && ['DisplayAsCard'].includes(choice.ability.displayAs)) return true
    if (tag === MessageType.TOOLTIP_LABEL) return true
    if (tag === MessageType.ABILITY_LABEL && choice.ability.type.tag === 'ConstantReaction') return true
    if (tag === MessageType.LABEL) return true
    if (tag === MessageType.INFO) return true
    if (tag === MessageType.INVALID_LABEL) return true
    if (tag === MessageType.SKILL_LABEL) return true
    if (tag === MessageType.SKILL_LABEL_WITH_LABEL) return true

    return false
  })
})
const choosePaymentAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('choosePaymentAmounts')
const chooseAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('chooseAmounts')
const question = computed(() => props.game.question[props.playerId])
const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)
const searchedCards = computed(() => {
  const playerCards = Object.entries(investigator.value?.foundCards ?? [])

  const playerZones = playerCards.filter(([, c]) => c.length > 0)

  const encounterCards = Object.entries(props.game.scenario?.foundCards ? props.game.scenario.foundCards : props.game.foundCards)
  const encounterZones = encounterCards.filter(([, c]) => c.length > 0)

  return [...playerZones, ...encounterZones]
})

const focusedCards = computed(() => {
  const {focusedCards, foundCards} = props.game

  if (focusedCards.length == 0) {
    if (Object.values(foundCards).some((v) => v.length > 0)) {
      return Object.values(foundCards).flat()
    }
  }

  const searchedCardIds = searchedCards.value.map(([, cards]) => {
    return cards.map((card) => toCardContents(card).id)
  }).flat()

  if (focusedCards.every((c) => searchedCardIds.includes(toCardContents(c).id))) {
    return []
  }


  return focusedCards
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
    return label(question.value.label)
  }

  return null
})

const amountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return label(question.value.label)
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

const readCards = computed(() => question.value?.readCards || [])

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

const paymentChoiceLabel = function(text: string): string {
  if (text.startsWith("$")) {
    return label(`$choice.${text.slice(1)}`)
  }

  return formatContent(text)
}

const hasInnerContent = computed(() => {
  return questionImage.value
    || (focusedCards.value.length > 0 && choices.value.length > 0)
    || (searchedCards.value.length > 0 && choices.value.length > 0)
    || paymentAmountsLabel.value
    || amountsLabel.value
    || (portraits.value.length > 0)
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

const questionImage = computed(() => {
  if (!question.value) {
    return null
  }

  if (question.value.tag !== 'QuestionLabel') {
    return null
  }

  if(question.value.card) {
    return cardLabelImage(question.value.card)
  }

  return null
})

const cardIdImage = (cardId: string) => {
  return (imgsrc(cardImage(props.game.cards[cardId])))
}

const portraitLabelImage = (investigatorId: string) => {
  const player = props.game.investigators[investigatorId]

  if (player.form.tag == "YithianForm") {
    return imgsrc(`portraits/${investigatorId.replace('c', '')}.jpg`)
  }

  if (player.form.tag == "HomunculusForm") {
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

const focusedTarotCards = computed(() => props.game.focusedTarotCards)

interface TarotChoice {
  card: ArkhamGame.TarotCard
  index?: number
}

const tarotChoices = computed<TarotChoice[]>(() => {
  const labels = tarotLabels.value
  if (labels.length == 0) return []
  if (focusedTarotCards.value.length == 0) return labels.map(({choice, index}) => ({ card: choice.tarotCard, index}))

  return focusedTarotCards.value.map((card) => {
    const choice = labels.find(({choice}) => choice.tarotCard.arcana === card.arcana)
    return choice ? { card, index: choice.index } : { card }
  })
})



const cardLabels = computed<{ choice: CardLabel, index: number}[]>(() =>
  choices.value.
    flatMap((choice, index) => {
      return choice.tag === "CardLabel" ? [{choice, index}] : []
    }))

const chaosTokenLabels = computed<{ choice: ChaosTokenLabel, index: number}[]>(() =>
  choices.value.
    flatMap((choice, index) => {
      return choice.tag === "ChaosTokenLabel" ? [{choice, index}] : []
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

const flippableCard = (cardCode: string) => {
  return {
    cardCode,
    doubleSided: true,
    classSymbols: [],
    cardType: 'UnknownType',
    art: cardCode.replace('c', ''),
    level: 0,
    traits: [],
    name: "",
    skills: [],
    cost: null,
    otherSide: `${cardCode}b`
  }
}

const cardFilter = useDebouncedRef('')
const store = useDbCardStore()

const filteredCards = computed<{ choice: CardLabel; index: number }[]>(() => {
  const q = cardFilter.value.trim().toLowerCase()
  if (q === '') return cardLabels.value

  return cardLabels.value.filter(({ choice }) => {
    const card = store.getDbCard(choice.cardCode.replace(/^c/, ''))
    if (!card) return false
    return card.name.toLowerCase().includes(q)
  })
})

</script>

<template>
  <div class='question-wrapper'>
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
      <div v-if="cardLabels.length > 10" class="filter">
        <input v-model="cardFilter" @keydown.stop placeholder="Filter" />
      </div>
      <template v-for="{choice, index} in filteredCards" :key="index">
        <CardImage v-if="choice.flippable" :card="flippableCard(choice.cardCode)" />
        <img v-else class="card" :src="cardLabelImage(choice.cardCode)" @click="choose(index)" />
      </template>
    </div>

    <div v-if="chaosTokenLabels.length > 0" class="cardLabels">
      <template v-for="{choice, index} in chaosTokenLabels" :key="index">
        <img
          class="token front"
          :src="chaosTokenImage(choice.face)"
          @click="choose(index)"
        />
      </template>
    </div>

    <div v-if="tarotChoices.length > 0" class="tarotLabels">
      <template v-for="{card, index} in tarotChoices" :key="index">
        <img v-if="index === undefined" class="card" :src="imgsrc(`tarot/${tarotCardImage(card)}`)" :class="{ [card.facing]: true}" />
        <a v-else href='#' @click.prevent="choose(index)">
          <img class="card" :src="imgsrc(`tarot/${tarotCardImage(card)}`)" :class="{ [card.facing]: true}" />
        </a>
      </template>
    </div>

    <div class="intro-text" v-if="question && question.tag === QuestionType.READ">
      <div v-if="readCards.length > 0" class="story-with-card">
        <img :src="imgsrc(`cards/${cardCode.replace('c', '')}.avif`)" v-for="cardCode in readCards" class="card no-overlay" />
        <div>
          <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
        </div>
      </div>
      <template v-else>
        <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
      </template>
    </div>


    <div class="question-label dropdown" v-if="question && question.tag === 'DropDown'">
      <div class="question-image" v-if="questionImage">
        <img :src="questionImage" class="card" />
      </div>

      <DropDown @choose="choose" :options="question.options" />
    </div>

    <template v-if="question && question.tag === 'ChooseExchangeAmounts'">
      <ExchangeTokens
        :game="game"
        :source="question.source"
        :investigator1="question.investigator1Id"
        :investigator1Amount="question.investigator1InitialAmount"
        :investigator2="question.investigator2Id"
        :investigator2Amount="question.investigator2InitialAmount"
        :token="question.token"
      />
    </template>

    <div class="question-label dropdown" v-if="question && question.tag === 'QuestionLabel' && question.question.tag === 'DropDown'">
      <div class="question-image" v-if="questionImage">
        <img :src="questionImage" class="card" />
      </div>

      <DropDown @choose="choose" :options="question.question.options" />
    </div>

    <div v-if="!isSkillTest && !inSkillTest && focusedChaosTokens.length > 0" class="tokens">
      <div class="question-image" v-if="questionImage">
        <img :src="questionImage" class="card" />
      </div>

      <Token v-for="(focusedToken, index) in focusedChaosTokens" :key="index" :token="focusedToken" :playerId="playerId" :game="game" @choose="choose" />
    </div>

    <div v-if="showChoices" class="choices">
      <div v-if="hasInnerContent" class="question-label">
        <div class="question-image" v-if="questionImage">
          <img :src="questionImage" class="card" />
        </div>

        <div class='question-content'>
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
                    :key="`${group}-${toCardContents(card).id}`"
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
                    :key="`${group}-${toCardContents(card).id}`"
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
                    <label :for="`choice-${paymentChoice.choiceId}`" v-html="paymentChoiceLabel(paymentChoice.label)"></label> <input type="number" :min="paymentChoice.minBound" :max="paymentChoice.maxBound" v-model.number="amountSelections[paymentChoice.choiceId]" :name="`choice-${paymentChoice.choiceId}`" onclick="this.select()" />
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

          <div v-if="questionImage && questionChoices.length > 0" class="choices-wrapper">
            <QuestionChoices :choices="questionChoices" :game="game" :playerId="playerId" @choose="choose" />
          </div>
        </div>
      </div>
      <QuestionChoices v-if="!questionImage" :choices="questionChoices" :game="game" :playerId="playerId" @choose="choose" />
    </div>
    <template v-else-if="question && question.tag === 'QuestionLabel' && question.question.tag !== 'DropDown'">
      <div v-if="questionImage" class="question-image">
        <img :src="questionImage" class="card" />
      </div>
    </template>
    <div v-if="doneLabel">
      <button class="done" @click="$emit('choose', doneLabel.index)" v-html="label(doneLabel.label)"></button>
    </div>
  </div>
</template>

<style scoped>
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
  &:deep(> div:only-child) {
    overflow-y: auto;
  }

  header {
    padding-inline: 20px;
    padding-bottom: 20px;
  }

  color: #222;
  max-width: 50vw;
  text-align: justify;
  background: linear-gradient(#DFDAD8, #c9c4c2);
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
  background-image: v-bind(grunge);
  background-size: cover;
  @media (max-width: 768px) {
    max-width: 100%;
    font-size: 1.2em;
    text-justify: inter-character;
  }
  &:deep(.resolution) {
    padding: 40px;
  }
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
      border-image-source: v-bind(checkpoint_fleur);
      border-image-slice: 49.9%;
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

  &[disabled] {
    cursor: not-allowed;
    background-color: #999 !important;
  }
}

button:hover {
  background-color: #311b3e;

  &[disabled] {
    background-color: #999 !important;
  }
}

.card {
  width: min-width(calc(var(--card-width) * 2));
  margin: 2px;
  height: max(calc(var(--card-width) * 2));
}

.question-label {
  display: flex;
  flex-direction: column;
  max-width: 75vw;
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

  .portrait {
    min-width: unset;
    width: calc(var(--card-width) / 2);
  }

  .card {
    height: max(var(--card-width)* 2);
  }
}

.card-pile:hover {
  background: rgba(255, 255, 255, 0.2);
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
  padding: 0px;
  padding-top: 10px;
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
  margin-top: 0.5em;
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

  .card {
    max-width: 200px;
  }
}

.tarotLabels {
  margin: 10px;
  img {
    border-radius: 5px;

    &.Reversed {
      transform: rotateZ(180deg);
      &:before {
        transform-origin: center;
        animation-fill-mode: forwards;
        animation: shadow-rotate 0.5s linear;
        transform: translate(0, -12px);
      }
    }
  }

  a > img {
    border: 1px solid var(--select);
  }

  .card {
    max-width: 200px;
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
  max-height: 60vh;
  > *:first-child {
    padding-top: 30px;
  }
  > ul, div {
    padding-inline: 20px;
  }
  > *:last-child {
    padding-bottom: 30px;

  }

  &:deep(> div.composite:first-of-type) {
    > div {
      padding-top: 30px;
    }
  }

  &:deep(> div.composite:last-of-type) {
    > div {
      padding-bottom: 30px;
    }
  }

  &:deep(> div.composite) {
    > div {
      padding-inline: 20px;
    }
  }


  &:has(.resolution) {
    background-color: #BAA597;
    box-shadow: unset;
    overflow: auto;
    isolation: isolate;
    position: relative;
    &::after {
      border: 20px solid #D4CCC3;
      border-left-width: 10px;
      border-right-width: 10px;
      position: absolute;
      inset: 0px;
      box-sizing: border-box;
      content: "";
      filter: blur(0.25em);
      z-index: -2;
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
    &::before {
      z-index: -1;
      pointer-events: none;
      position: absolute;
      inset: 10px;
      border-image-source: v-bind(resolution_fleur);
      border-image-slice: 49.9%;
      border-image-repeat: no-repeat;
      border-image-width: 50px;
      content: "";
    }
    @media (max-width: 800px) and (orientation: portrait)  {
      padding: 10px;
      &::before {
        border-image-width: 20px;
      }
    }
  }
  &:has(.black) {
    background-color: rba(0,0,0,0.8);
    box-shadow: unset;
    overflow: hidden;
    &::after {
      border: 20px solid #000;
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
      border-bottom: 1px solid #000;
      &::after {
        border-bottom: 1px solid #000;
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
      border-image-source: v-bind(black_fleur);
      border-image-slice: 49.9%;
      border-image-repeat: no-repeat;
      border-image-width: 50px;
      content: "";
    }
    @media (max-width: 800px) and (orientation: portrait)  {
      padding: 10px;
      &::before {
        border-image-width: 20px;
      }
    }
  }
}

.question-image {
  display: flex;
  justify-content: center;
}

.card {
  flex-basis: 30%;
  flex-shrink: 0;
  height: fit-content;
  border-radius: 15px;
}

.story-with-card {
  display: flex;
  gap: 10px;
}

.question-label:has(> .question-image) {
  display: flex;
  flex-direction: row;
  flex-wrap: nowrap;
  align-items: flex-start;
  align-self: center;
  height: fit-content;
  gap: 10px;
  width: 100%;
  border-radius: 15px;
  h2 {
    color: white;
    text-transform: uppercase;
    font-size: 1.8em;
    background: var(--neutral-extra-dark);
    border-top-left-radius: 15px;
    border-top-right-radius: 15px;
    font-family: Teutonic, sans-serif;
    padding: 10px 20px;
  }
  > .question-image {
    justify-content: flex-start;
    img  {
      width: calc(var(--card-width) * 4);
      flex-basis: unset;
      flex-shrink: unset;
    }
  }
  > .question-content {
    background-color: rgba(0,0,0,0.3);
    border-radius: 15px;
    display: flex;
    flex-direction: column;
    height: 100%;
    gap: 10px;
    flex: 1;
    .portraits {
      justify-content: flex-start;
      align-items: flex-start;
      padding: 10px;
      display: flex;
      gap: 10px;
    }
    .portrait {
      display: flex;
      margin: 0;
      img { min-width: fit-content; };
    }
    .portrait-choices {
      justify-items: flex-start;
      flex: 1;
    }
  }
  button {
    width: 100%;
  }
}

.question-content {
  @media (max-width: 800px) and (orientation: portrait)  {
    :deep(.card) {
      width: 10.71vw ;
      height: 14.994vw;
      max-width: 10.71vw;
    }
  }
}

.active {
  border: 1px solid var(--select);
}

.choices-wrapper {
  flex: 1;
  display: flex;
  flex-direction: column;
  width: 100%;
  box-sizing: border-box;
  gap: 10px;
  padding: 10px;
  button {
    border: 1px solid #444;
  }
  border-radius: inherit;
  border: 1px solid #444;
}

.question-wrapper {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.question-wrapper:has(> .question-image) .question-image{
  width: var(--card-width);
  align-self: center;
  justify-self: center;
  img {
    flex-basis: 100%;
    height: auto;
    border-radius: 3px;
  }
}

.filter {
  padding-bottom: 10px;
  width: 100%;
  input {
    width: 100%;
  }
}
</style>
