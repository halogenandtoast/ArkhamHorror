<script lang="ts" setup>
import { useDbCardStore } from '@/stores/dbCards'
import { chaosTokenImage } from '@/arkham/types/ChaosToken';
import { useI18n } from 'vue-i18n';
import { useDebouncedRef } from '@/composable/debouncedRef';
import { handleEmbeddedI18n } from '@/arkham/i18n';
import { formatCost } from '@/arkham/cost';
import { choiceRequiresModal, MessageType, CardLabel, ChaosTokenLabel, type Message, type TargetLabel } from '@/arkham/types/Message';
import { computed, inject, ref, watch, onMounted } from 'vue';
import { imgsrc, formatContent } from '@/arkham/helpers';
import { cardArt, cardImage as cardCodeImage, investigatorPortrait } from '@/arkham/cardImages';
import { AmountChoice, QuestionType, amountTargetUnmet } from '@/arkham/types/Question';
import Card from '@/arkham/components/Card.vue';
import * as ArkhamGame from '@/arkham/types/Game';
import { tarotCardImage } from '@/arkham/types/TarotCard';
import { cardImage, toCardContents, type Card as ArkhamCard, type CardContents } from '@/arkham/types/Card';
import DropDown from '@/components/DropDown.vue';
import Token from '@/arkham/components/Token.vue';
import type { Game } from '@/arkham/types/Game';
import ExchangeTokens from '@/arkham/components/ExchangeTokens.vue';
import ChaosBagChoice from '@/arkham/components/ChaosBagChoice.vue';
import FormattedEntry from '@/arkham/components/FormattedEntry.vue';
import QuestionChoices from '@/arkham/components/QuestionChoices.vue';
import CardImage from '@/arkham/components/CardImage.vue';

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
    case "FromDeck": return t("fromDeck")
    case "FromHand": return t("fromHand")
    case "FromDiscard": return t("fromDiscard")
    case "FromEncounterDeck": return t("fromEncounterDeck")
    case "FromEncounterDiscard": return t("fromEncounterDiscard")
    default: return s
  }
}
const inSkillTest = computed(() => props.game.skillTest !== null)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))
const toChoiceEntry = (c: Message, idx: number): [Message, number] => [c, idx]
const questionChoices = computed(() => {
  const withoutDone = choices.value.map(toChoiceEntry).filter(([choice, _]) => {
    const { tag } = choice
    if (tag === MessageType.ABILITY_LABEL) return !abilityLabelHandledElsewhere(choice)
    if (tag === MessageType.TARGET_LABEL) return !targetLabelHandledElsewhere(choice)
    if (tag === MessageType.TOOLTIP_LABEL) return true
    if (tag === MessageType.LABEL) return true
    if (tag === MessageType.INFO) return true
    if (tag === MessageType.INVALID_LABEL) return true
    if (tag === MessageType.SKILL_LABEL) return true
    if (tag === MessageType.SKILL_LABEL_WITH_LABEL) return true
    if (tag === MessageType.COST_LABEL) return true

    return false
  })

  // When Done appears alongside regular label choices, include it so it renders
  // with the same button style instead of the modal-footer style
  if (withoutDone.length > 0 && doneLabel.value) {
    const doneEntry = choices.value.map(toChoiceEntry).find(([c]) => c.tag === MessageType.DONE)
    if (doneEntry) return [...withoutDone, doneEntry]
  }

  return withoutDone
})
const choosePaymentAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('choosePaymentAmounts')
const chooseAmounts = inject<(amounts: Record<string, number>) => Promise<void>>('chooseAmounts')
const question = computed(() => props.game.question[props.playerId])
const focusedChaosTokens = computed(() => props.game.focusedChaosTokens)

type SearchedCardGroup = {
  key: string
  zone: string
  label: string
  cards: ArkhamCard[]
}

function searchedZoneLabel(zone: string, source: 'player' | 'encounter') {
  if (source === 'encounter') {
    switch (zone) {
      case 'FromDeck': return t('fromEncounterDeck')
      case 'FromDiscard': return t('fromEncounterDiscard')
    }
  }

  return zoneToLabel(zone)
}

const searchedCards = computed<SearchedCardGroup[]>(() => {
  const playerCards = Object.entries(investigator.value?.foundCards ?? {})
    .filter(([, cards]) => cards.length > 0)
    .map(([zone, cards]) => ({ key: `player-${zone}`, zone, label: searchedZoneLabel(zone, 'player'), cards }))

  const encounterCards = Object.entries({
    ...(props.game.scenario?.foundCards ?? {}),
    ...props.game.foundCards,
  })
    .filter(([, cards]) => cards.length > 0)
    .map(([zone, cards]) => ({ key: `encounter-${zone}`, zone, label: searchedZoneLabel(zone, 'encounter'), cards }))

  return [...playerCards, ...encounterCards]
})

const focusedCards = computed(() => {
  if (searchedCards.value.length > 0) {
    return []
  }

  return props.game.focusedCards
})

function zoneTag(zone: unknown): string | null {
  if (typeof zone === 'string') return zone
  if (zone && typeof zone === 'object' && 'tag' in zone) return String((zone as { tag: unknown }).tag)
  return null
}

function cardContentsId(card: CardContents): string {
  return card.id
}

function focusedCardSourceLabel(cardId: string): string | null {
  if (props.game.scenario?.discard.some((card) => cardContentsId(card) === cardId)) {
    return t('fromEncounterDiscard')
  }

  if (props.game.scenario?.encounterDeck.some((card) => cardContentsId(card) === cardId)) {
    return t('fromEncounterDeck')
  }

  for (const [, [deck, discard]] of Object.entries(props.game.scenario?.encounterDecks ?? {})) {
    if (deck.some((card) => cardContentsId(card) === cardId)) return t('fromEncounterDeck')
    if (discard.some((card) => cardContentsId(card) === cardId)) return t('fromEncounterDiscard')
  }

  const choice = choices.value.find((choice) => {
    return choice.tag === MessageType.TARGET_LABEL
      && choice.target.tag === 'CardIdTarget'
      && choice.target.contents === cardId
  })

  if (!choice || choice.tag !== MessageType.TARGET_LABEL) return null

  const messages = 'messages' in choice && Array.isArray(choice.messages) ? choice.messages : []
  const foundMessage = messages.find((message) => {
    return typeof message === 'object' && message !== null && 'tag' in message && message.tag === 'FoundEncounterCardFrom'
  })
  const zone = zoneTag(
    foundMessage && typeof foundMessage === 'object' && 'contents' in foundMessage && Array.isArray(foundMessage.contents)
      ? foundMessage.contents[2]
      : null
  )
  if (!zone) return null

  switch (zone) {
    case 'FromEncounterDeck': return t('fromEncounterDeck')
    case 'FromEncounterDiscard': return t('fromEncounterDiscard')
    case 'FromDiscard': return t('fromEncounterDiscard')
    case 'FromDeck': return t('fromEncounterDeck')
    default: return zoneToLabel(zone)
  }
}

const focusedCardGroups = computed<SearchedCardGroup[]>(() => {
  if (focusedCards.value.length === 0) return []

  const grouped = new Map<string, ArkhamCard[]>()
  for (const card of focusedCards.value) {
    const label = focusedCardSourceLabel(toCardContents(card).id) ?? t('cards')
    grouped.set(label, [...(grouped.get(label) ?? []), card])
  }

  return Array.from(grouped.entries()).map(([label, cards]) => ({
    key: `focused-${label}`,
    zone: label,
    label,
    cards,
  }))
})

const visibleCardIds = computed(() => new Set([
  ...(investigator.value?.hand ?? []).map((card) => toCardContents(card).id),
  ...focusedCards.value.map((card) => toCardContents(card).id),
  ...searchedCards.value.flatMap((group) => group.cards.map((card) => toCardContents(card).id)),
  ...(props.game.scenario?.victoryDisplay ?? []).map((card) => toCardContents(card).id),
  ...Object.values(props.game.assets).flatMap((asset) => asset.cardsUnderneath.map((card) => toCardContents(card).id)),
]))

function abilityLabelHandledElsewhere(choice: Message) {
  if (choice.tag !== MessageType.ABILITY_LABEL) return false

  const source = choice.ability.source
  if (source.sourceTag === 'ProxySource') {
    return source.source.tag === 'CardCodeSource'
      ? abilitySourceHandledElsewhere(source.originalSource)
      : abilitySourceHandledElsewhere(source.source)
  }

  return abilitySourceHandledElsewhere(source)
}

function abilitySourceHandledElsewhere(source: any) {
  if (typeof source.contents !== 'string') return false

  switch (source.tag) {
    case 'AssetSource': return source.contents in props.game.assets
    case 'LocationSource': return source.contents in props.game.locations
    case 'EnemySource': return source.contents in props.game.enemies
    case 'TreacherySource': return source.contents in props.game.treacheries
    case 'ActSource': return source.contents in props.game.acts
    case 'AgendaSource': return source.contents in props.game.agendas
    case 'EventSource': return source.contents in props.game.events || visibleCardIds.value.has(source.contents)
    case 'StorySource': return source.contents in props.game.stories
    case 'InvestigatorSource': return source.contents in props.game.investigators || source.contents in props.game.otherInvestigators
    default: return false
  }
}

function targetLabelHandledElsewhere(choice: TargetLabel) {
  const target = choice.target
  const contents = target.contents

  if (typeof contents === 'string') {
    switch (target.tag) {
      case 'AssetTarget': return contents in props.game.assets
      case 'LocationTarget': return contents in props.game.locations
      case 'EnemyTarget': return contents in props.game.enemies
      case 'TreacheryTarget': return contents in props.game.treacheries
      case 'ActTarget': return contents in props.game.acts
      case 'AgendaTarget': return contents in props.game.agendas
      case 'EventTarget': return contents in props.game.events
      case 'StoryTarget': return contents in props.game.stories
      case 'SkillTarget': return contents in props.game.skills
      case 'InvestigatorTarget': return contents in props.game.investigators || contents in props.game.otherInvestigators
      case 'ScarletKeyTarget': return contents in props.game.scarletKeys
      case 'ConcealedTarget': return contents in props.game.concealed
      case 'CardIdTarget': return visibleCardIds.value.has(contents)
      case 'ChaosTokenFaceTarget': return props.game.focusedChaosTokens.some((token) => token.face === contents)
      default: return false
    }
  }

  if (target.tag === 'ChaosTokenTarget' && typeof contents === 'object' && contents !== null && 'id' in contents) {
    return props.game.focusedChaosTokens.some((token) => token.id === contents.id)
  }

  return false
}

const isRead = computed(() => question.value?.tag === QuestionType.READ)

const showChoices = computed(() => {
  if (props.game.skillTest && !props.isSkillTest && !isRead.value) {
    return false
  }
  if (choices.value.some(choiceRequiresModal)) {
    return true
  }
  return props.game.focusedChaosTokens.length > 0 || focusedCards.value.length > 0 || searchedCards.value.length > 0 || paymentAmountsLabel.value || amountsLabel.value
})

const label = function(body: string) {
  return formatContent(handleEmbeddedI18n(body, t))
}

const paymentAmountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return label(question.value.label)
  }

  if (question.value?.tag === QuestionType.PAY_COST_QUESTION && question.value.question.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return label(t('label.cost.pay', { cost: formatCost(question.value.cost, t) }))
  }

  return null
})

const amountsLabel = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return label(question.value.label)
  }

  if (question.value?.tag === QuestionType.QUESTION_LABEL && question.value?.question?.tag === QuestionType.CHOOSE_AMOUNTS) {
    return label(question.value.question.label)
  }

  return null
})

const paymentAmountsChoices = computed(() => {
  if (question.value?.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.value.paymentAmountChoices
  }

  if (question.value?.tag === QuestionType.PAY_COST_QUESTION && question.value.question.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return question.value.question.paymentAmountChoices
  }

  return []
})

const readCards = computed(() => question.value?.readCards || [])

const suppressReadInSkillTest = computed(() => props.isSkillTest && isRead.value)

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
    const choice = choices.value[doneIndex]
    return choice.tag === MessageType.DONE ? { label: choice.label, index: doneIndex } : null
  }

  return null
})

const doneIsFooter = computed(() => !questionChoices.value.some(([c]) => c.tag === MessageType.DONE))

const paymentChoiceLabel = function(text: string): string {
  if (text.startsWith("$")) {
    return label(`$choice.${text.slice(1)}`)
  }

  return formatContent(text)
}

const traumaKind = (text: string) => {
  const normalized = text.toLowerCase()
  if (normalized.includes('physical')) return 'health'
  if (normalized.includes('mental')) return 'horror'
  return null
}

const traumaIcon = (text: string) => {
  switch (traumaKind(text)) {
    case 'health': return imgsrc('health-icon.png')
    case 'horror': return imgsrc('horror-icon.png')
    default: return null
  }
}

const traumaIconStyle = (text: string) => {
  const icon = traumaIcon(text)
  if (!icon) return {}
  return {
    maskImage: `url(${icon})`,
    WebkitMaskImage: `url(${icon})`,
  }
}

const hasInnerContent = computed(() => {
  return questionImage.value
    || (focusedCards.value.length > 0 && choices.value.length > 0)
    || (searchedCards.value.length > 0 && choices.value.length > 0)
    || paymentAmountsLabel.value
    || amountsLabel.value
    || (portraits.value.length > 0)
})

onMounted(() => {
  setInitialAmounts()
  void store.initDbCards()
})

watch(
  () => props.game.question[props.playerId],
  setInitialAmounts)

const unmetAmountRequirements = computed(() => {
  const q = question.value
  if (!q) return true
  const total = Object.values(amountSelections.value).reduce((a, b) => a + b, 0)

  if (q.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return amountTargetUnmet(q.paymentAmountTargetValue, total)
  }
  if (q.tag === QuestionType.PAY_COST_QUESTION && q.question.tag === QuestionType.CHOOSE_PAYMENT_AMOUNTS) {
    return amountTargetUnmet(q.question.paymentAmountTargetValue, total)
  }
  if (q.tag === QuestionType.CHOOSE_AMOUNTS) {
    return amountTargetUnmet(q.amountTargetValue, total)
  }
  if (q.tag === QuestionType.QUESTION_LABEL && q.question.tag === QuestionType.CHOOSE_AMOUNTS) {
    return amountTargetUnmet(q.question.amountTargetValue, total)
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

const cardLabelImage = (cardCode: string) => cardCodeImage(cardCode)

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
  const card = props.game.cards[cardId]
  return card ? imgsrc(cardImage(card)) : ''
}

const portraitLabelImage = (investigatorId: string) => investigatorPortrait(props.game, investigatorId)

const portraits = computed<[Message, number][]>(() =>
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
    art: cardArt(cardCode),
    level: 0,
    cardTraits: [],
    name: { title: '', subtitle: null },
    skills: [],
    cost: null,
    otherSide: `${cardCode}b`,
    meta: {}
  }
}

const cardFilter = useDebouncedRef('')
const store = useDbCardStore()

const filteredCards = computed<{ choice: CardLabel; index: number }[]>(() => {
  const q = cardFilter.value.trim().toLowerCase()
  if (q === '') return cardLabels.value

  return cardLabels.value.filter(({ choice }) => {
    const card = store.getDbCard(cardArt(choice.cardCode))
    if (!card) return false
    return card.name.toLowerCase().includes(q)
  })
})

</script>

<template>
  <div class='question-wrapper'>
    <ChaosBagChoice v-if="chaosBagChoice" :choice="chaosBagChoice" :game="game" :playerId="playerId" @choose="choose" />
    <div v-if="cardPiles.length > 0" class="cardPiles">
      <div v-for="{pile, index} in cardPiles" :key="index" class="card-pile" @click="choose(index)">
        <div v-for="card in pile" :key="`${card.cardId}-${card.cardOwner ?? ''}`" class="pile-card">
          <img class="card" :src="cardIdImage(card.cardId)" />
          <img v-if="card.cardOwner" class="portrait" :src="portraitLabelImage(card.cardOwner)" />
        </div>
      </div>
    </div>

    <div v-if="cardLabels.length > 0" class="cardLabels">
      <div v-if="cardLabels.length > 10" class="filter">
        <input v-model="cardFilter" @keydown.stop :placeholder="$t('questionFilter.filter')" />
      </div>
      <template v-for="{choice, index} in filteredCards" :key="index">
        <CardImage v-if="choice.flippable" :card="flippableCard(choice.cardCode)" />
        <img v-else class="card" :src="cardLabelImage(choice.cardCode)" @click="choose(index)" />
      </template>
    </div>

    <div v-if="chaosTokenLabels.length > 0" class="chaosTokenLabels">
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

    <div class="intro-text" v-if="question && question.tag === QuestionType.READ && !suppressReadInSkillTest">
      <div v-if="readCards.length > 0" class="story-with-card">
        <img :src="cardCodeImage(cardCode)" v-for="cardCode in readCards" :key="cardCode" class="card no-overlay" />
        <div>
          <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
        </div>
      </div>
      <div v-else class="intro-text-body">
        <FormattedEntry v-for="(paragraph, index) in question.flavorText.body" :key="index" :entry="paragraph" />
      </div>
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

    <div class="question-label dropdown" v-if="question && question.tag === 'PayCostQuestion' && question.question.tag === 'DropDown'">
      <div class="question-image" v-if="questionImage">
        <img :src="questionImage" class="card" />
      </div>

      <legend>{{ t('label.cost.pay', { cost: formatCost(question.cost, t) }) }}</legend>
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
          <div v-if="focusedCardGroups.length > 0 && choices.length > 0" class="modal">
            <div class="modal-contents searched-cards focused-cards">
              <div v-for="group in focusedCardGroups" :key="group.key" class="group">
                <h2>{{ group.label }}</h2>
                <div class="group-cards">
                  <div
                    v-for="card in group.cards"
                    :key="`${group.key}-${toCardContents(card).id}`"
                    class="searched-card"
                  >
                    <Card
                      :card="card"
                      :game="game"
                      :playerId="playerId"
                      @choose="$emit('choose', $event)"
                    />
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div v-if="searchedCards.length > 0 && choices.length > 0" class="modal">
            <div class="modal-contents searched-cards">
              <div v-for="group in searchedCards" :key="group.key" class="group">
                <h2>{{ group.label }}</h2>
                <div class="group-cards">
                  <div
                    v-for="card in group.cards"
                    :key="`${group.key}-${toCardContents(card).id}`"
                    class="searched-card"
                  >
                    <Card
                      :card="card"
                      :game="game"
                      :playerId="playerId"
                      @choose="$emit('choose', $event)"
                    />
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div v-if="paymentAmountsLabel" class="modal amount-modal">
            <div class="modal-contents amount-contents">
              <form class="amount-form" @submit.prevent="submitPaymentAmounts" :disabled="unmetAmountRequirements">
                <legend v-html="paymentAmountsLabel"></legend>
                <div class="amount-choice-list">
                  <template v-for="amountChoice in paymentAmountsChoices" :key="amountChoice.choiceId">
                    <div v-if="amountChoice.maxBound !== 0" class="amount-choice">
                      <label :for="`payment-choice-${amountChoice.choiceId}`">{{ amountChoice.title }}</label>
                      <span class="amount-input-wrapper">
                        <span
                          v-if="traumaIcon(amountChoice.title)"
                          class="amount-input-icon"
                          :class="`amount-input-icon--${traumaKind(amountChoice.title)}`"
                          :style="traumaIconStyle(amountChoice.title)"
                        ></span>
                        <input
                          :id="`payment-choice-${amountChoice.choiceId}`"
                          class="amount-input"
                          :class="{ 'with-icon': traumaIcon(amountChoice.title) }"
                          type="number"
                          :min="amountChoice.minBound"
                          :max="amountChoice.maxBound"
                          v-model.number="amountSelections[amountChoice.choiceId]"
                          onclick="this.select()"
                        />
                      </span>
                    </div>
                  </template>
                </div>
                <button class="amount-submit" :disabled="unmetAmountRequirements">{{ t('submit') }}</button>
              </form>
            </div>
          </div>
          <div v-if="amountsLabel" class="modal amount-modal">
            <div v-if="searchedCards.length > 0" class="modal-contents searched-cards">
              <div v-for="group in searchedCards" :key="group.key" class="group">
                <h2>{{ group.label }}</h2>
                <div class="group-cards">
                  <div
                    v-for="card in group.cards"
                    :key="`${group.key}-${toCardContents(card).id}`"
                    class="searched-card"
                  >
                    <Card
                      :card="card"
                      :game="game"
                      :playerId="playerId"
                      @choose="$emit('choose', $event)"
                    />
                  </div>
                </div>
              </div>
            </div>
            <div class="modal-contents amount-contents">
              <form class="amount-form" @submit.prevent="submitAmounts" :disabled="unmetAmountRequirements">
                <legend v-html="amountsLabel"></legend>
                <div class="amount-choice-list">
                  <template v-for="paymentChoice in chooseAmountsChoices" :key="paymentChoice.choiceId">
                    <div v-if="paymentChoice.maxBound !== 0" class="amount-choice">
                      <label :for="`choice-${paymentChoice.choiceId}`" v-html="paymentChoiceLabel(paymentChoice.label)"></label>
                      <span class="amount-input-wrapper">
                        <span
                          v-if="traumaIcon(paymentChoice.label)"
                          class="amount-input-icon"
                          :class="`amount-input-icon--${traumaKind(paymentChoice.label)}`"
                          :style="traumaIconStyle(paymentChoice.label)"
                        ></span>
                        <input
                          :id="`choice-${paymentChoice.choiceId}`"
                          class="amount-input"
                          :class="{ 'with-icon': traumaIcon(paymentChoice.label) }"
                          type="number"
                          :min="paymentChoice.minBound"
                          :max="paymentChoice.maxBound"
                          v-model.number="amountSelections[paymentChoice.choiceId]"
                          :name="`choice-${paymentChoice.choiceId}`"
                          onclick="this.select()"
                        />
                      </span>
                    </div>
                  </template>
                </div>
                <button class="amount-submit" :disabled="unmetAmountRequirements">{{ t('submit') }}</button>
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
    <div v-if="doneLabel && doneIsFooter">
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
  background-color: var(--neutral-dark);
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

  color: var(--neutral-extra-dark);
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
    padding: 20px;
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
      z-index: var(--z-index-1);
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
      z-index: var(--z-index-2);
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
  &:has(.haunted) {
    background: #0e0f06;
    color: #c1c49c;
    border: 0;
    border-radius: 0;
    max-width: none;
    max-height: none;
    width: 100%;
    height: 100%;
    flex: 1;
    margin: 0;
    padding: 40px 32px;
    position: relative;
    overflow: hidden;
    isolation: isolate;

    &::before {
      content: "";
      position: absolute;
      inset: -120px;
      pointer-events: none;
      background:
        radial-gradient(ellipse at 50% 40%, #3a3d16 0%, #1f2110 55%, #0e0f06 80%);
      background-image: v-bind(grunge), radial-gradient(ellipse at 50% 40%, #3a3d16 0%, #1f2110 55%, #0e0f06 80%);
      background-blend-mode: overlay;
      background-size: cover;
      z-index: var(--z-index-0);
    }

    &::after {
      content: "";
      position: absolute;
      inset: -120px;
      pointer-events: none;
      background:
        radial-gradient(circle at 25% 75%, rgba(131, 137, 56, 0.16), transparent 55%),
        radial-gradient(circle at 75% 25%, rgba(131, 137, 56, 0.12), transparent 55%);
      z-index: var(--z-index-0);
      animation: haunted-flicker 7s ease-in-out infinite;
    }

    .intro-text-body, &:deep(.intro-text-body) {
      position: relative;
      z-index: var(--z-index-1);
      margin: -40px;
    }
  }
}

@keyframes haunted-flicker {
  0%, 100% { filter: brightness(1); }
  3% { filter: brightness(0.78); }
  6% { filter: brightness(1.05); }
  9% { filter: brightness(0.85); }
  12% { filter: brightness(1); }
  62% { filter: brightness(1); }
  64% { filter: brightness(0.7); }
  66% { filter: brightness(1.02); }
  68% { filter: brightness(1); }
}

.status-bar {
  text-align: center;
}

button {
  transition: all 0.3s ease-in;
  border: 0;
  padding: 10px;
  background-color: var(--button-2);
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
  background-color: var(--neutral-extra-dark);
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
      background-color: var(--button-2);
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

.question-label:has(.amount-modal),
.question-content:has(.amount-modal) {
  width: 100%;
  box-sizing: border-box;
}

.amount-modal {
  width: 100%;
  box-sizing: border-box;
}

.amount-contents {
  width: 100%;
  box-sizing: border-box;
  padding: 0;
  overflow: hidden;
  align-items: stretch;
  background: #735e7b;
  border: 1px solid rgba(255, 255, 255, 0.18);
  border-radius: 18px;
}

.amount-form {
  display: flex;
  flex: 1;
  flex-direction: column;
  gap: 16px;
  width: 100%;
  padding: 22px;
  box-sizing: border-box;
}

.amount-contents legend {
  width: 100%;
  color: #fff;
  font-size: 1.25em;
  font-weight: 800;
  line-height: 1.25;
  text-align: center;
  text-wrap: balance;
  margin: 0;
  padding: 0 6px 6px;
}

.amount-choice-list {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  gap: 10px;
  width: 100%;
}

.amount-choice {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 5.5rem;
  flex: 0 0 calc(50% - 5px);
  max-width: calc(50% - 5px);
  gap: 12px;
  align-items: center;
  min-width: 0;
  padding: 12px 14px;
  background: rgba(255, 255, 255, 0.11);
  border: 1px solid rgba(255, 255, 255, 0.16);
  border-radius: 14px;
  box-sizing: border-box;
}

.amount-choice:only-child {
  flex-basis: 100%;
  max-width: 100%;
}

.amount-choice label {
  color: #f6edf8;
  font-size: 1.05em;
  font-weight: 700;
  line-height: 1.25;
  text-align: left;
}

.amount-input-wrapper {
  position: relative;
  display: block;
  min-width: 0;
}

.amount-input-icon {
  position: absolute;
  top: 50%;
  left: 0.55em;
  width: 1.1em;
  height: 1.1em;
  transform: translateY(-50%);
  pointer-events: none;
  mask-repeat: no-repeat;
  mask-position: center;
  mask-size: contain;
  -webkit-mask-repeat: no-repeat;
  -webkit-mask-position: center;
  -webkit-mask-size: contain;
}

.amount-input-icon--health {
  background-color: #d44;
}

.amount-input-icon--horror {
  background-color: #1f6fbf;
}

.amount-input {
  width: 100%;
  min-width: 0;
  padding: 0.55em 0.65em;
  color: #241a29;
  background: #f7f0f8;
  border: 1px solid rgba(36, 26, 41, 0.25);
  border-radius: 11px;
  font-size: 1.05em;
  font-weight: 800;
  text-align: center;
  box-sizing: border-box;
}

.amount-input.with-icon {
  padding-left: 2em;
}

.amount-input::-webkit-inner-spin-button,
.amount-input::-webkit-outer-spin-button {
  opacity: 1;
}

.amount-input:focus {
  outline: 2px solid #d7b7df;
  outline-offset: 2px;
}

.amount-submit {
  width: 100%;
  margin-top: 2px;
  padding: 0.8em 1em;
  background: #3f2f48;
  border: 1px solid rgba(255, 255, 255, 0.14);
  border-radius: 13px;
  color: white;
  text-align: center;
}

.amount-submit:hover {
  background: #2f2238;
}

.amount-submit[disabled] {
  cursor: not-allowed;
}

.choices {
  text-align: center;
}

.choices button {
  transition: all 0.3s ease-in;
  border: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: var(--button-2);
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
  align-items: stretch;
  gap: 12px;
  width: 100%;
  overflow-x: auto;
}

.group {
  display: flex;
  align-items: stretch;
  flex-direction: column;
  gap: 10px;
  width: 100%;
  box-sizing: border-box;
  padding: 12px;
  border: 1px solid rgba(214, 205, 174, 0.18);
  border-radius: 10px;
  background: rgba(20, 16, 24, 0.74);
  box-shadow:
    inset 0 0 0 1px rgba(0, 0, 0, 0.35),
    0 6px 18px rgba(0, 0, 0, 0.24);
}

.group h2 {
  margin: -2px -2px 2px;
  padding-bottom: 7px;
  border-bottom: 1px solid rgba(214, 205, 174, 0.16);
  color: var(--title);
  font-family: "Teutonic", serif;
  font-size: 1.05rem;
  font-weight: 400;
  letter-spacing: 0.04em;
  line-height: 1;
  text-transform: uppercase;
  text-shadow: 0 1px 2px rgba(0, 0, 0, 0.7);
}

.group .group-cards {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  padding-top: 2px;
}

.searched-card {
  display: flex;
}

.done {
  width: 100%;
  border: 0;
  margin: 0;
  padding: 10px;
  text-transform: uppercase;
  background-color: var(--button-2);
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
    height: auto;
  }

  .card {
    max-width: 200px;
  }
}

.chaosTokenLabels {
  margin: 10px;
  img {
    width: 10vw;
    max-width: 150px;
    border: 1px solid var(--select);
    border-radius: 100vw;
  }
}

.tarotLabels {
  margin: 10px;
  img {
    border-radius: 5px;
    height: auto;

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

  a {
    display: inline-block;
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
    height: auto;
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
    overflow: hidden;
    max-height: none;
    isolation: isolate;
    position: relative;
    .intro-text-body {
      max-height: 60vh;
      overflow-y: auto;
    }
    &::after {
      border: 20px solid #D4CCC3;
      border-left-width: 10px;
      border-right-width: 10px;
      position: absolute;
      inset: 0px;
      box-sizing: border-box;
      content: "";
      filter: blur(0.25em);
      z-index: var(--z-index-neg-2);
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
      z-index: var(--z-index-neg-1);
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
      z-index: var(--z-index-1);
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
      z-index: var(--z-index-2);
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
      height: auto;
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
      img {
        width: auto;
        height: max(calc(var(--card-width) * 2));
      };

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
    border: 1px solid var(--button-highlight);
  }
  border-radius: inherit;
  border: 1px solid var(--button-highlight);
}

.question-wrapper {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.question-wrapper:has(.haunted) {
  gap: 0;

  :deep(.question-choices) {
    gap: 0;
    padding: 0 !important;
  }

  :deep(.message-label) {
    margin: 0;
    padding: 0;
  }

  .done,
  :deep(.question-choices button),
  :deep(.question-choices a.button) {
    background:
      linear-gradient(180deg, #14181b 0%, #0a0d10 100%);
    color: #c9d2a8;
    border: 0;
    border-radius: 0 0 16px 16px;
    margin: 0;
    text-shadow:
      0 0 6px rgba(135, 156, 90, 0.55),
      0 1px 2px rgba(0, 0, 0, 0.9);
    letter-spacing: 0.08em;
    font-family: Teutonic, "Noto Sans", sans-serif;
    transition: background 200ms ease, color 200ms ease;

    &:hover {
      background:
        linear-gradient(180deg, #1a2620 0%, #0a0d10 100%);
      color: #dfe6c4;
    }
  }
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
