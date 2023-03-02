<script lang="ts" setup>
import { computed, inject, ref } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Card } from '@/arkham/types/Card'
import * as ArkhamGame from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import AbilityButton from '@/arkham/components/AbilityButton.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import * as Arkham from '@/arkham/types/Agenda';

export interface Props {
  agenda: Arkham.Agenda
  game: Game
  cardsUnder: Card[]
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['show', 'choose'])

const id = computed(() => props.agenda.id)
const baseUrl = inject('baseUrl')
const image = computed(() => {
  if (props.agenda.flipped) {
    return `${baseUrl}/img/arkham/cards/${id.value.replace('c', '')}b.jpg`;
  }

  return `${baseUrl}/img/arkham/cards/${id.value.replace('c', '')}.jpg`;
})

const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

const viewingUnder = ref(false)
const viewUnderLabel = computed(() => viewingUnder.value ? "Close" : `${props.cardsUnder.length} Cards Underneath`)

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL && c.target.contents === id.value) {
    return true
  }
  return false
}

const interactAction = computed(() => choices.value.findIndex(canInteract));

function isAbility(v: Message) {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { tag } = v.ability.source;

  if (tag === 'ProxySource') {
    return v.ability.source.source.contents === id.value
  } else if (tag === 'AgendaSource') {
    return v.ability.source.contents === id.value
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
const showCardsUnderAgenda = (e: Event) => emit('show', e, cardsUnder, 'Cards Under Agenda', false)

const debug = inject('debug')
const debugChoose = inject('debugChoose')
</script>

<template>
  <div class="agenda-container">
    <img
      :class="{ 'agenda--can-progress': interactAction !== -1 }"
      class="card card--sideways"
      @click="$emit('choose', interactAction)"
      :src="image"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability"
      :ability="choices[ability]"
      :data-image="image"
      @click="$emit('choose', ability)"
      />
    <Treachery
      v-for="treacheryId in agenda.treacheries"
      :key="treacheryId"
      :treachery="game.treacheries[treacheryId]"
      :game="game"
      :investigatorId="investigatorId"
      @choose="$emit('choose', $event)"
    />
    <div class="pool">
      <PoolItem
        type="doom"
        :amount="agenda.doom"
      />

      <template v-if="debug">
        <button @click="debugChoose({tag: 'PlaceDoom', contents: [{'tag': 'AgendaTarget', 'contents': id}, 1]})">+</button>
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
