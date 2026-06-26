<script lang="ts" setup>
import { computed, ComputedRef } from 'vue';
import { Game } from '@/arkham/types/Game';
import { Card } from '@/arkham/types/Card';
import * as ArkhamGame from '@/arkham/types/Game';
import { AbilityLabel, AbilityMessage, Message, MessageType } from '@/arkham/types/Message';
import { cardImage } from '@/arkham/cardImages';
import TokenPool from '@/arkham/components/TokenPool.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Token from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Event';

export interface Props {
  game: Game
  event: Arkham.Event
  playerId: string
  attached?: boolean
}

const props = withDefaults(defineProps<Props>(), { attached: false })

const emits = defineEmits<{
  choose: [value: number]
  showCards: [e: Event, cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
}>()

const id = computed(() => props.event.id)
const hasPool = computed(() => {
  const { sealedChaosTokens, tokens } = props.event
  return sealedChaosTokens.length > 0 || Object.values(tokens).some((amount) => (amount ?? 0) > 0)
})

const cardCode = computed(() => props.event.cardCode)
const image = computed(() => {
  const mutated = props.event.mutated ? `_${props.event.mutated}` : ''
  return cardImage(cardCode.value, mutated)
})
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value || c.target.contents === props.event.cardId
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))

const exhausted = computed(() => props.event.exhausted)

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if("contents" in source.source) {
      return source.source.contents === id.value
    }
  }

  if (source.tag === 'EventSource') {
    return source.contents === id.value
  }

  if (source.tag === 'CardIdSource') {
    return source.contents === props.event.cardId
  }

  return false
}

const abilities = computed(() => {
  return choices.value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, displayAsAction: false, index: i }];
      }

      return acc;
    }, []);
})

const cardsUnderneath = computed(() => props.event.cardsUnderneath)
const cardsUnderneathLabel = computed(() => `Underneath (${cardsUnderneath.value.length})`)

const showCardsUnderneath = (e: Event) => emits('showCards', e, cardsUnderneath, "Cards Underneath", false)

const choose = (index: number) => emits('choose', index)
</script>

<template>
  <div class="event" :class="{ attached }">
    <img
      :src="image"
      :class="{ 'event--can-interact': cardAction !== -1, exhausted, attached }"
      class="card event"
      @click="$emit('choose', cardAction)"
      :data-customizations="JSON.stringify(event.customizations)"
    />
    <div v-if="hasPool" class="pool">
      <TokenPool :tokens="event.tokens" />
      <Token
        v-for="(sealedToken, index) in event.sealedChaosTokens"
        :key="index"
        :token="sealedToken"
        :playerId="playerId"
        :game="game"
        @choose="choose"
      />
    </div>
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      :game="game"
      @click="$emit('choose', ability.index)"
      />

    <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
  </div>
</template>

<style scoped>
.card {
  width: var(--card-width);
  max-width: var(--card-width);
  border-radius: 5px;
}

.event {
  display: flex;
  flex-direction: column;
  position: relative;
}

.event--can-interact {
  border: 2px solid var(--select);
  cursor:pointer;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid var(--select);
}

:deep(.token) {
  width: 40px;
}

.pool {
  position: absolute;
  top: 50%;
  align-items: center;
  display: flex;
  align-self: flex-start;
  align-items: flex-end;
  :deep(.token-container) {
    width: unset;
  }
  :deep(img) {
    width: var(--card-token-width);
    height: auto;
  }
  z-index: var(--z-index-1);
  pointer-events: none;
}

.attached .card {
  object-fit: cover;
  object-position: left bottom;
  height: calc(var(--card-width)*0.6);
}

.exhausted {
  transition: transform 0.2s linear;
  transform: rotate(90deg);
  padding: 0 30px;
}
</style>
