<script lang="ts" setup>
import { useDebug } from '@/arkham/debug'
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message'
import * as ArkhamCard from '@/arkham/types/Card';
import * as Arkham from '@/arkham/types/Investigator'
import * as ArkhamGame from '@/arkham/types/Game';
import type { Game } from '@/arkham/types/Game'
import {computed, ref, watch} from 'vue'
import { imgsrc } from '@/arkham/helpers';
import { cardImage } from '@/arkham/cardImages';
import { useI18n } from 'vue-i18n';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import Card from '@/arkham/components/Card.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import CardsUnderIndicator from '@/arkham/components/CardsUnderIndicator.vue';

const { t } = useI18n();

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
    return cardImage(topCard.cardCode)
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
        .some(discardItem => choice.tag === 'TargetLabel' && ArkhamCard.toCardContents(discardItem).id === choice.target.contents)
    )
})


const topOfDeckTreachery = computed(() => {
  const mTreacheryId = Object.values(props.game.treacheries).
    filter((t) => t.placement.tag === "OnTopOfDeck" && t.placement.contents === id.value).
    map((t) => t.id)[0]
  return mTreacheryId ? props.game.treacheries[mTreacheryId] : null
})

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

type DeckDropMode = 'shuffle' | 'top' | 'bottom'

const deckDropMode = ref<DeckDropMode | null>(null)
const deckDropPosition = ref<{ x: number; y: number } | null>(null)

function deckModeFromEvent(event: DragEvent): DeckDropMode {
  if (event.shiftKey) return 'top'
  if (event.altKey) return 'bottom'
  return 'shuffle'
}

const deckDropIndicator = computed(() => {
  switch (deckDropMode.value) {
    case 'top': return { icon: '↑', label: 'Place on top' }
    case 'bottom': return { icon: '↓', label: 'Place on bottom' }
    case 'shuffle': return { icon: '↻', label: 'Shuffle in' }
    default: return null
  }
})

function onDropDeck(event: DragEvent) {
  event.preventDefault()
  deckDropMode.value = null
  deckDropPosition.value = null
  if (!debug.active) return
  if (!event.dataTransfer) return
  const data = event.dataTransfer.getData('text/plain')
  if (!data) return
  const json = JSON.parse(data)
  if (json.tag !== 'CardTarget') return
  const target = { tag: 'CardIdTarget', contents: json.contents }
  const deckSig = { tag: 'InvestigatorDeck', contents: id.value }
  const mode = deckModeFromEvent(event)
  if (mode === 'top') {
    debug.send(props.game.id, { tag: 'PutOnTopOfDeck', contents: [id.value, deckSig, target] })
  } else if (mode === 'bottom') {
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

function onDragOverDeck(event: DragEvent) {
  dragover(event)
  if (debug.active) {
    deckDropMode.value = deckModeFromEvent(event)
    deckDropPosition.value = { x: event.clientX, y: event.clientY }
  }
}

function onDragLeaveDeck(event: DragEvent) {
  const target = event.currentTarget
  const related = event.relatedTarget
  if (target instanceof Node && related instanceof Node && target.contains(related)) return
  deckDropMode.value = null
  deckDropPosition.value = null
}

const canSelectDraw = computed(() => {
  return Object.entries(props.investigator.foundCards).length == 0
})

const discards = computed<ArkhamCard.Card[]>(() => props.investigator.discard.map(c => { return { tag: 'PlayerCard', contents: c }}))
const discardPopoverShown = ref(false)

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

  if (newChoices.length > 0 && newChoices.every(isDiscardChoice)) {
    discardPopoverShown.value = true
  }
}, { immediate: true })

</script>

<template>
  <div class="discard"
    @drop="onDropDiscard($event)"
    @dragover.prevent="dragover($event)"
    @dragenter.prevent
  >
    <Card v-if="topOfDiscard" :class="{'discard--can-use': discardCardsAction === true}" :game="game" :card="topOfDiscard" :playerId="playerId" />
    <CardsUnderIndicator
      v-if="discards.length > 0"
      class="view-discard-button"
      :cards="discards"
      :game="game"
      :playerId="playerId"
      v-model:shown="discardPopoverShown"
      :label="t('investigator.discards')"
      :isDiscards="true"
      :highlighted="discardCardsAction"
      :fullWidth="true"
      @choose="emit('choose', $event)"
    />
    <button v-if="debug.active && discards.length > 0" class="view-discard-button" @click="debug.send(game.id, {tag: 'ShuffleDiscardBackIn', contents: investigatorId})">{{ $t('draw.shuffleBackIn') }}</button>
  </div>
  <div class="deck-container">
    <div
      class="top-of-deck"
      :class="{ 'top-of-deck--drop-target': deckDropIndicator }"
      @drop="onDropDeck($event)"
      @dragover.prevent="onDragOverDeck($event)"
      @dragleave="onDragLeaveDeck($event)"
      @dragend="deckDropMode = null; deckDropPosition = null"
      @dragenter.prevent="onDragOverDeck($event)"
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
      <div
        v-if="deckDropIndicator && deckDropPosition"
        class="deck-drop-indicator"
        :class="`deck-drop-indicator--${deckDropMode}`"
        :style="{ left: `${deckDropPosition.x}px`, top: `${deckDropPosition.y}px` }"
      >
        <span class="deck-drop-indicator__icon">{{ deckDropIndicator.icon }}</span>
        <span class="deck-drop-indicator__label">{{ deckDropIndicator.label }}</span>
      </div>
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
      <button v-if="canSelectDraw" @click="debug.send(game.id, {tag: 'SearchMessage', contents: {tag: 'Search_', contents: ['Looking', investigatorId, {tag: 'GameSource', contents: []}, { tag: 'InvestigatorTarget', contents: investigatorId }, [[{tag: 'FromDeck', contents: []}, 'ShuffleBackIn']], {tag: 'BasicCardMatch', contents: {tag: 'AnyCard', contents: []}}, { tag: 'DrawFound', contents: [investigatorId, 1]}]}})">{{ $t('draw.selectDraw') }}</button>
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

.view-discard-button.cards-under-indicator {
  display: flex;
  width: var(--card-width);
  max-width: var(--card-width);

  @media (max-width: 800px) and (orientation: portrait) {
    width: calc(var(--pool-token-width)*1.2);
    max-width: calc(var(--pool-token-width)*1.2);
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

.top-of-deck--drop-target .deck {
  outline: 3px solid var(--select);
  outline-offset: 3px;
}

.deck-drop-indicator {
  position: fixed;
  display: inline-flex;
  flex-direction: row;
  align-items: center;
  justify-content: center;
  gap: 6px;
  padding: 5px 8px;
  border-radius: 999px;
  border: 1px solid color-mix(in srgb, var(--select) 45%, rgba(255, 255, 255, 0.3));
  background: rgba(0, 0, 0, 0.46);
  color: rgba(255, 255, 255, 0.92);
  pointer-events: none;
  z-index: 2147483647;
  transform: translate(18px, -50%);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.28);
  backdrop-filter: blur(2px);
  text-shadow: 0 1px 2px rgba(0, 0, 0, 0.75);
}

.deck-drop-indicator__icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 18px;
  height: 18px;
  border-radius: 50%;
  background: color-mix(in srgb, var(--select) 45%, transparent);
  border: 1px solid rgba(255, 255, 255, 0.48);
  font-size: 0.9rem;
  font-weight: 700;
  line-height: 18px;
  text-align: center;
  font-family: system-ui, sans-serif;
}

.deck-drop-indicator__label {
  font-size: 0.72rem;
  font-weight: 700;
  white-space: nowrap;
}

</style>
