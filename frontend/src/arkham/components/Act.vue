<script lang="ts" setup>
import { computed, ref, inject } from 'vue'
import type { Game } from '@/arkham/types/Game'
import type { Card } from '@/arkham/types/Card'
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import PoolItem from '@/arkham/components/PoolItem.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import * as ArkhamGame from '@/arkham/types/Game'
import type { Message } from '@/arkham/types/Message'
import { MessageType } from '@/arkham/types/Message'
import * as Arkham from '@/arkham/types/Act'

export interface Props {
  act: Arkham.Act
  game: Game
  cardsUnder: Card[]
  cardsNextTo: Card[]
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['show', 'choose'])
const baseUrl = inject('baseUrl')

const id = computed(() => props.act.id)
const image = computed(() => {
  const side = props.act.sequence.side.toLowerCase().replace('a', '')
  return `${baseUrl}/img/arkham/cards/${id.value.replace('c', '')}${side}.jpg`
})

const imageForCard = (card: Card) => {
  const side = card.contents.isFlipped ? 'b' : ''
  // TODO, send art with cards next to
  const art = card.contents.art || card.contents.cardCode.replace('c', '')
  return `${baseUrl}/img/arkham/cards/${art}${side}.jpg`
}

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const viewingUnder = ref(false)
const viewUnderLabel = computed(() => viewingUnder.value ? "Close" : `${props.cardsUnder.length} Cards Underneath`)

function canInteract(c: Message): boolean {
  if (c.tag === "TargetLabel" && c.target.contents === id.value) {
    return true
  }
  return false
}

const interactAction = computed(() => choices.value.findIndex(canInteract));

function isAbility(v: Message) {
  if (v.tag !== 'AbilityLabel') {
    return false
  }

  const { tag, contents } = v.ability.source;

  if (tag === 'ActSource' && contents === id.value) {
    return true
  }

  if (tag === 'ProxySource' && contents[0].tag === 'ActSource' && contents[0].contents === id.value) {
    return true
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<number[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, i];
      }

      return acc;
    }, [])
})

const cardsUnder = computed(() => props.cardsUnder)
const showCardsUnderAct = (e: Event) => emit('show', e, cardsUnder, 'Cards Under Act', false)

const debug = inject('debug')
const debugChoose = inject('debugChoose')
</script>

<template>
  <div class="act-container">
    <img
      :class="{ 'act--can-progress': interactAction !== -1 }"
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
      v-for="treacheryId in act.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />

    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      :data-image="image"
      @click="$emit('choose', ability)"
      />

    <template v-if="debug">
      <button @click="debugChoose({tag: 'AdvanceAct', contents: [id, {tag: 'TestSource', contents:[]}]})">Advance</button>
    </template>

    <button v-if="cardsUnder.length > 0" class="view-cards-under-button" @click="showCardsUnderAct">{{viewUnderLabel}}</button>

    <div class="pool">
      <PoolItem
        v-if="act.clues && act.clues > 0"
        type="clue"
        :amount="act.clues"
      />
    </div>
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

.act-container {
  display: flex;
  flex-direction: column;
}

.card--sideways {
  width: auto;
  height: $card-width;
}

.act--can-progress {
  border: 3px solid #ff00ff;
  border-radius: 8px;
  cursor: pointer;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

.ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}
</style>
