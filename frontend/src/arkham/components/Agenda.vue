<script lang="ts" setup>
import { ComputedRef, computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import { imgsrc } from '@/arkham/helpers';
import { type Game } from '@/arkham/types/Game';
import { type Card, cardImage } from '@/arkham/types/Card'
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, type Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import * as Arkham from '@/arkham/types/Agenda';


const props = defineProps<{
  agenda: Arkham.Agenda
  game: Game
  cardsUnder: Card[]
  cardsNextTo: Card[]
  playerId: string
}>()

const emit = defineEmits<{
  show: [cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
  choose: [value: number]
}>()

const id = computed(() => props.agenda.id)
const image = computed(() => {
  const suffix = props.agenda.flipped ? 'b' : ''
  return imgsrc(`cards/${id.value.replace('c', '')}${suffix}.jpg`);
})

const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

const viewingUnder = ref(false)
const viewUnderLabel = computed(() => viewingUnder.value ? "Close" : `${props.cardsUnder.length} Cards Underneath`)

function canInteract(c: Message): boolean {
  return c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value
}

const interactAction = computed(() => choices.value.findIndex(canInteract));

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'AgendaSource') {
    if ("contents" in source) {
      return source.contents === id.value
    }
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }]
      }

      return acc
    }, [])
})

const cardsUnder = computed(() => props.cardsUnder)
const showCardsUnderAgenda = () => emit('show', cardsUnder, 'Cards Under Agenda', false)

const imageForCard = (card: Card) => {
  return imgsrc(cardImage(card))
}

const treacheries = computed(() => Object.values(props.game.treacheries).
  filter((t) => t.placement.tag === "TreacheryNextToAgenda").
  map((t) => t.id))


const debug = useDebug()
</script>

<template>
  <div class="agenda-container">
    <img
      :class="{ 'agenda--can-progress': interactAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', interactAction)"
      :src="image"
    />
    <img
      v-for="(card, idx) in cardsNextTo"
      class="card card--sideways"
      :key="idx"
      :src="imageForCard(card)"
    />
    <Treachery
      v-for="treacheryId in treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="$emit('choose', ability.index)"
      />
    <Treachery
      v-for="treacheryId in agenda.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :playerId="playerId"
      @choose="$emit('choose', $event)"
    />
    <div class="pool">
      <PoolItem
        type="doom"
        :amount="agenda.doom"
      />

      <template v-if="debug.active">
        <button @click="debug.send(game.id, {tag: 'PlaceTokens', contents: [{'tag': 'GameSource'}, {'tag': 'AgendaTarget', 'contents': id}, 'Doom', 1]})">+</button>
      </template>
    </div>

    <button v-if="cardsUnder.length > 0" class="view-cards-under-button" @click="showCardsUnderAgenda">{{viewUnderLabel}}</button>
  </div>
</template>

<style scoped lang="scss">
.card {
  width: $card-width;
  -webkit-box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.23), 0 3px 6px rgba(0, 0, 0, 0.53);
  border-radius: 6px;
  margin: 2px;
}

.card--sideways {
  width: auto;
  height: $card-width;
}

.agenda-container {
  display: flex;
  flex-direction: column;
}

.agenda--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
  justify-content: flex-start;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.agenda :deep(.treachery) {
  object-fit: cover;
  object-position: 0 -74px;
  height: 68px;
  margin-top: 2px;
}
</style>
