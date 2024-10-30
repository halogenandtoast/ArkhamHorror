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
  if (props.agenda.flipped) {
    return imgsrc(`cards/${id.value.replace('c', '').replace(/a$/, '')}b.avif`);
  }
  return imgsrc(`cards/${id.value.replace('c', '')}.avif`);
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

const nextToTreacheries = computed(() => Object.values(props.game.treacheries).
  filter((t) => t.placement.tag === "NextToAgenda").
  map((t) => t.id))

function groupBy<T, K extends string | number | symbol>(
  array: T[],
  getKey: (item: T) => K
): Record<K, T[]> {
  return array.reduce((result, item) => {
    const key = getKey(item);
    if (!result[key]) {
      result[key] = [];
    }
    result[key].push(item);
    return result;
  }, {} as Record<K, T[]>);
}

const groupedTreacheries = computed(() => Object.entries(groupBy([...props.agenda.treacheries, ...nextToTreacheries.value], (t) => props.game.treacheries[t].cardCode)))

const debug = useDebug()
</script>

<template>
  <div class="agenda-container">
    <div class="agenda-card">
      <img
        :class="{ 'agenda--can-progress': interactAction !== -1 }"
        class="card card--sideways card--agenda"
        @click="$emit('choose', interactAction)"
        :src="image"
      />
      <div class="pool" v-if="!agenda.flipped">
        <template v-if="debug.active">
          <button @click="debug.send(game.id, {tag: 'RemoveTokens', contents: [{'tag': 'GameSource'}, {'tag': 'AgendaTarget', 'contents': id}, 'Doom', 1]})">-</button>
        </template>

        <PoolItem
          type="doom"
          :amount="agenda.doom"
        />

        <template v-if="debug.active">
          <button @click="debug.send(game.id, {tag: 'PlaceTokens', contents: [{'tag': 'GameSource'}, {'tag': 'AgendaTarget', 'contents': id}, 'Doom', 1]})">+</button>
        </template>
      </div>
    </div>
    <img
      v-for="(card, idx) in cardsNextTo"
      class="card card--sideways"
      :key="idx"
      :src="imgsrc(cardImage(card))"
    />
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      class="sideways"
      @click="$emit('choose', ability.index)"
      />
    <div v-if="groupedTreacheries.length > 0" class="treacheries">
      <div v-for="([cCode, treacheries], idx) in groupedTreacheries" :key="cCode" class="treachery-group" :style="{ zIndex: (groupedTreacheries.length - idx) * 10 }">
        <div v-for="treacheryId in treacheries" class="treachery-card" :key="treacheryId" >
          <Treachery
            :treachery="game.treacheries[treacheryId]"
            :game="game"
            :playerId="playerId"
            @choose="$emit('choose', $event)"
            :overlay-delay="310"
          />
        </div>
      </div>
    </div>

    <button v-if="cardsUnder.length > 0" class="view-cards-under-button" @click="showCardsUnderAgenda">{{viewUnderLabel}}</button>
  </div>
</template>

<style scoped lang="scss">
.card {
  width: var(--card-width);
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
  border-radius: 6px;
}

.card--sideways {
  width: auto;
  height: var(--card-width);
  max-width: max-content;
}

.card--agenda {
  z-index: 100;
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

.treacheries {
  position:relative;
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.treachery-group {
  display: flex;
  gap: 5px;
  flex-direction: row;
  margin-top: -50px;
  //position: inherit;
  transition: margin-top 0.3s;
  position: relative;

  &:hover {
    margin-top: 0px;
    .treachery-card {
      margin-left: 0;
    }
  }
}

.treachery {
  box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.45);
}

.treachery-card {
  margin-left: -50px;
  transition: margin-left 0.3s;
  &:first-of-type {
    margin-left: 0;
  }
}

.agenda-card {
  > img {
    box-shadow: 1px 1px 6px rgba(0, 0, 0, 0.8);
  }
  z-index: 100;
  position: relative;
  .pool {
    z-index: 101;
    position: absolute;
    left: 0;
    top: 0;
  }
}

.agenda-container :deep(.card--sideways) {
  width: auto;
  height: var(--card-width);
  aspect-ratio: var(--card-sideways-aspect);
}
</style>
