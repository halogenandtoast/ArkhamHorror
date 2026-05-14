<script lang="ts" setup>
import { useDebug } from '@/arkham/debug'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import * as ArkhamCard from '@/arkham/types/Card';
import * as Arkham from '@/arkham/types/Investigator'
import * as ArkhamGame from '@/arkham/types/Game';
import type { Game } from '@/arkham/types/Game'
import {computed, ComputedRef, ref, reactive, watch} from 'vue'
import { imgsrc, pluralize } from '@/arkham/helpers';
import { useI18n } from 'vue-i18n';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import Card from '@/arkham/components/Card.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import CardRow from '@/arkham/components/CardRow.vue';

const { t } = useI18n();

interface RefWrapper<T> {
  ref: ComputedRef<T>
}

export interface Props {
  game: Game
  investigator: Arkham.Investigator
  playerId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['choose'])
const investigatorId = computed(() => props.investigator.id)
const debug = useDebug()

const id = computed(() => props.investigator.id)
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const topOfDiscard = computed(() => discards.value[0])

const topOfDeckRevealed = computed(() =>
  props.investigator.modifiers?.some((m) => m.type.tag === "OtherModifier" && m.type.contents === "TopCardOfDeckIsRevealed")
)

const topOfDeck = computed(() => {
  const topCard = props.investigator.deck[0]
  if  (topOfDeckRevealed.value && topCard) {
    return imgsrc(`cards/${topCard.cardCode.replace(/^c/, '')}.avif`)
  }
  return imgsrc("player_back.jpg")
})

const playTopOfDeckAction = computed(() => {
  if(props.playerId !== props.investigator.playerId) {
    return -1
  }
  const topOfDeck = props.investigator.deck[0]
  if (topOfDeck !== undefined && topOfDeck !== null && topOfDeckTreachery.value === null) {
    return choices.value.findIndex((c) => c.tag === "TargetLabel" && c.target.contents === props.investigator.deck[0].id)
  }
  return -1
})

function isTopOfDeckAbility(v: Message, cardId: string): v is AbilityLabel {
  if (v.tag !== 'AbilityLabel') return false
  const { source } = v.ability
  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === cardId
    }
    return false
  }
  if (source.tag === 'CardIdSource') return source.contents === cardId
  if (source.tag === 'EventSource') return source.contents === cardId
  if (source.tag === 'SkillSource') return source.contents === cardId
  if (source.tag === 'AssetSource') return source.contents === cardId
  return false
}

const topOfDeckAbilities = computed<AbilityMessage[]>(() => {
  if (!topOfDeckRevealed.value) return []
  const topCard = props.investigator.deck[0]
  if (!topCard) return []
  const cardId = topCard.id
  return choices.value.reduce<AbilityMessage[]>((acc, v, i) => {
    if (isTopOfDeckAbility(v, cardId)) {
      return [...acc, { contents: v, displayAsAction: false, index: i }]
    }
    return acc
  }, [])
})

const viewingDiscard = ref(false)
const viewDiscardLabel = computed(() => viewingDiscard.value ? t('close') : pluralize(t('scenario.discardCard'), discards.value.length))

const drawCardsAction = computed(() => {
  if(props.playerId !== props.investigator.playerId) {
    return -1
  }
  return choices
    .value
    .findIndex((c) => {
      if (c.tag === "ComponentLabel") {
        return (c.component.tag == "InvestigatorDeckComponent")
      }
      return false
    });
})

const discardCardsAction = computed(() => {
  return choices
    .value
    .some(choice => 
      discards
        .value
        .some(discardItem => discardItem.contents.id === choice.target?.contents)
    )
})

const noCards = computed<ArkhamCard.Card[]>(() => [])

// eslint-disable-next-line
const showCards = reactive<RefWrapper<any>>({ ref: noCards })
const cardRowTitle = ref("")

const topOfDeckTreachery = computed(() => {
  const mTreacheryId = Object.values(props.game.treacheries).
    filter((t) => t.placement.tag === "OnTopOfDeck" && t.placement.contents === id.value).
    map((t) => t.id)[0]
  return mTreacheryId ? props.game.treacheries[mTreacheryId] : null
})

const hideCards = () => {
  showCards.ref = noCards
  viewingDiscard.value = false
}

function onDropDiscard(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    const data = event.dataTransfer.getData('text/plain')
    if (data) {
      const json = JSON.parse(data)
      if (json.tag === "CardTarget") {
        debug.send(props.game.id, {tag: 'DiscardCard', contents: [id.value, {'tag': 'GameSource' }, json.contents]})
      }
    }
  }
}

function onDropDeck(event: DragEvent) {
  event.preventDefault()
  if (!debug.active) return
  if (!event.dataTransfer) return
  const data = event.dataTransfer.getData('text/plain')
  if (!data) return
  const json = JSON.parse(data)
  if (json.tag !== 'CardTarget') return
  const target = { tag: 'CardIdTarget', contents: json.contents }
  const deckSig = { tag: 'InvestigatorDeck', contents: id.value }
  if (event.shiftKey) {
    debug.send(props.game.id, { tag: 'PutOnTopOfDeck', contents: [id.value, deckSig, target] })
  } else if (event.altKey) {
    debug.send(props.game.id, { tag: 'PutOnBottomOfDeck', contents: [id.value, deckSig, target] })
  } else {
    debug.send(props.game.id, { tag: 'ShuffleIntoDeck', contents: [deckSig, target] })
  }
}

const dragover = (e: DragEvent) => {
  e.preventDefault()
  if (e.dataTransfer) {
    e.dataTransfer.dropEffect = 'copy'
  }
}

const canSelectDraw = computed(() => {
  return Object.entries(props.investigator.foundCards).length == 0
})

const doShowCards = (event: Event, cards: ComputedRef<ArkhamCard.Card[]>, title: string, isDiscards: boolean) => {
  cardRowTitle.value = title
  showCards.ref = cards
  viewingDiscard.value = isDiscards
}
const showDiscards = (e: Event) => doShowCards(e, discards, t('investigator.discards'), true)
const discards = computed<ArkhamCard.Card[]>(() => props.investigator.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))

const forcedShowDiscard = ref(false)
watch(choices, async (newChoices) => {
  const isDiscardChoice = (c: Message) => {
    if (c.tag === "TargetLabel") {
      if (c.target.tag !== "CardIdTarget") return false
      return props.investigator.discard.some(card => card.id === c.target.contents)
    }
    if (c.tag === "AbilityLabel") {
      const sourceId = c.ability.source.sourceTag === 'OtherSource' ? c.ability.source.contents : undefined
      if (!sourceId) return false
      if (props.investigator.discard.some(card => card.id === sourceId)) return true
      const asset = props.game.assets[sourceId]
      return asset && props.investigator.discard.some(card => asset.cardId == card.id)
    }
    return false
  }
  const showDiscard = newChoices.length > 0 && newChoices.every(isDiscardChoice)
  if (showDiscard) {
    showDiscards(new CustomEvent('showDiscards'))
    forcedShowDiscard.value = true
  } else {
    if (!forcedShowDiscard.value) return
    showCards.ref = noCards
    forcedShowDiscard.value = false
  }
}, { immediate: true })

</script>

<template>
  <div class="discard"
    @drop="onDropDiscard($event)"
    @dragover.prevent="dragover($event)"
    @dragenter.prevent
    @click="showDiscards"
  >
    <Card v-if="topOfDiscard" :class="{'discard--can-use': discardCardsAction === true}" :game="game" :card="topOfDiscard" :playerId="playerId" />
    <button v-if="discards.length > 0" class="view-discard-button" @click="showDiscards">{{viewDiscardLabel}}</button>
    <button v-if="debug.active && discards.length > 0" class="view-discard-button" @click="debug.send(game.id, {tag: 'ShuffleDiscardBackIn', contents: investigatorId})">{{ $t('draw.shuffleBackIn') }}</button>
  </div>
  <CardRow
    v-if="showCards.ref.length > 0"
    :game="game"
    :playerId="playerId"
    :cards="showCards.ref"
    :isDiscards="viewingDiscard"
    :title="cardRowTitle"
    @choose="emit('choose', $event)"
    @close="hideCards"
  />
  <div class="deck-container">
    <div
      class="top-of-deck"
      @drop="onDropDeck($event)"
      @dragover.prevent="dragover($event)"
      @dragenter.prevent
    >
      <Treachery
        v-if="topOfDeckTreachery"
        :treachery="topOfDeckTreachery"
        :game="game"
        :data-index="topOfDeckTreachery.cardId"
        :playerId="playerId"
        class="deck"
        @choose="emit('choose', $event)"
      />
      <img
        v-else
        :class="{ 'deck--can-draw': drawCardsAction !== -1, 'card': topOfDeckRevealed }"
        class="deck"
        :src="topOfDeck"
        width="150px"
        @click="emit('choose', drawCardsAction)"
      />
      <span class="deck-size">{{investigator.deckSize}}</span>
      <button v-if="playTopOfDeckAction !== -1" @click="emit('choose', playTopOfDeckAction)">{{ $t('label.play') }}</button>
      <AbilityButton
        v-for="ability in topOfDeckAbilities"
        :key="ability.index"
        :ability="ability.contents"
        :game="game"
        @click="emit('choose', ability.index)"
      />
    </div>
    <template v-if="debug.active">
      <button v-if="canSelectDraw" @click="debug.send(game.id, {tag: 'Search', contents: ['Looking', investigatorId, {tag: 'GameSource', contents: []}, { tag: 'InvestigatorTarget', contents: investigatorId }, [[{tag: 'FromDeck', contents: []}, 'ShuffleBackIn']], {tag: 'BasicCardMatch', contents: {tag: 'AnyCard', contents: []}}, { tag: 'DrawFound', contents: [investigatorId, 1]}]})">{{ $t('draw.selectDraw') }}</button>
      <button @click="debug.send(game.id, {tag: 'ShuffleDeck', contents: {tag: 'InvestigatorDeck', contents: investigatorId}})">{{ $t('draw.shuffle') }}</button>
    </template>
  </div>
</template>

<style scoped>

.discard {
  cursor: pointer;
  button {
    white-space: nowrap;
    text-wrap: pretty;
  }

  @media (max-width: 800px) and (orientation: portrait) {
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    button {
      width: fit-content;
    }
  }

  @media (min-width: 801px) {
    width: var(--card-width);
    :deep(button) {
      display: block;
    }
  }

  &:deep(.card) {
    margin: 0;
    box-shadow: none;
    filter: grayscale(.85);
    width: var(--card-width);
    @media (max-width: 800px) and (orientation: portrait)  {
      width: calc(var(--pool-token-width)*1.2);
    }
  }

  &:deep(.card-container) {
    margin: 0;
    position: relative;
    display: inline-flex;

  }
}

.discard--can-use{
  &::before {
    content: '';
    position: absolute;
    border-radius: 6px;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;      
    z-index: 1;
    box-shadow: inset 0 0 0 2px var(--select);
  }
}

.deck, .card {
  border-radius: 6px;
  max-width: var(--card-width);
  @media (max-width: 800px) and (orientation: portrait)  {
    max-width: calc(var(--pool-token-width)*1.2);
  }
}

.card-container {
  display: flex;
  flex-direction: column;
}

.view-discard-button {
  width: 100%;
  @media (max-width: 800px) and (orientation: portrait) {
    width: fit-content;
  }
}

.deck-container {
  display: flex;
  flex-direction: column;
  @media (max-width: 800px) and (orientation: portrait)  {
    margin-left: auto;
  }
}

.deck--can-draw {
  border: 2px solid var(--select);
  border-radius: 6px;
  cursor: pointer;
}

.deck-size {
  background: rgba(0, 0, 0, 0.6);
  padding: 5px;
  border-radius: 20px;
  pointer-events: none;
  position: absolute;
  font-weight: bold;
  color: var(--title);
  inset: 0;
  width: fit-content;
  height: fit-content;
  aspect-ratio: 1;
  line-height: 1;
  margin: auto;
  transform: translateY(-28.0%);
}

.top-of-deck {
  position: relative;
  display: flex;
  flex-direction: column;
  width: fit-content;
}

</style>
