<script lang="ts" setup>
import { ComputedRef, computed } from 'vue';
import { useDebug } from '@/arkham/debug';
import { TokenType } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import Key from '@/arkham/components/Key.vue';
import Event from '@/arkham/components/Event.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Token from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Asset';
import { Card } from '../types/Card';

const props = defineProps<{
  game: Game
  asset: Arkham.Asset
  investigatorId: string
}>()

const emit = defineEmits<{
  choose: [value: number]
  showCards: [e: Event, cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
}>()

const id = computed(() => props.asset.id)
const hasPool = computed(() => {
  const {
    sanity,
    health,
    uses,
    tokens,
    sealedChaosTokens,
    keys,
  } = props.asset;
  return sanity || health || tokens[TokenType.Damage] || tokens[TokenType.Horror] || uses || (tokens[TokenType.Doom] || 0) > 0 || (tokens[TokenType.Clue] || 0) > 0 || (tokens[TokenType.Resource] || 0) > 0 || sealedChaosTokens.length > 0 || keys.length > 0;
})

const exhausted = computed(() => props.asset.exhausted)
const cardCode = computed(() => props.asset.cardCode)
const image = computed(() => {
  return imgsrc(`cards/${cardCode.value.replace('c', '')}.jpg`)
})
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value || c.target.contents === props.asset.cardId
  }

  return false
}

function canAdjustHealth(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "AssetComponent" && c.component.tokenType === "DamageToken") {
    return c.component.assetId === id.value
  }
  return false
}

function canAdjustSanity(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "AssetComponent" && c.component.tokenType === "HorrorToken") {
    return c.component.assetId === id.value
  }
  return false
}

const cardAction = computed(() => choices.value.findIndex(canInteract))
const healthAction = computed(() => choices.value.findIndex(canAdjustHealth))
const sanityAction = computed(() => choices.value.findIndex(canAdjustSanity))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if ("contents" in source.source) {
      return source.source.contents === id.value
    }
  } else if (source.tag === 'AssetSource') {
    return source.contents === id.value
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<AbilityMessage[]>((acc, v, i) => {
      if (isAbility(v)) {
        return [...acc, { contents: v, index: i }];
      }

      return acc;
    }, []);
})

const cardsUnderneath = computed(() => props.asset.cardsUnderneath)
const cardsUnderneathLabel = computed(() => `Underneath (${cardsUnderneath.value.length})`)

const showCardsUnderneath = (e: Event) => emit('showCards', e, cardsUnderneath, "Cards Underneath", false)

const keys = computed(() => props.asset.keys)

const debug = useDebug()

const doom = computed(() => props.asset.tokens[TokenType.Doom])
const clues = computed(() => props.asset.tokens[TokenType.Clue])
const resources = computed(() => props.asset.tokens[TokenType.Resource])

const damage = computed(() => props.asset.tokens[TokenType.Damage])
const horror = computed(() => props.asset.tokens[TokenType.Horror])

const choose = (idx: number) => emit('choose', idx)
</script>

<template>
  <div class="asset">
    <img
      :src="image"
      :class="{ 'asset--can-interact': cardAction !== -1, exhausted}"
      class="card"
      @click="choose(cardAction)"
    />
    <Event
      v-for="eventId in asset.events"
      :event="game.events[eventId]"
      :game="game"
      :investigatorId="investigatorId"
      :key="eventId"
      @choose="$emit('choose', $event)"
    />
    <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
    <AbilityButton
      v-for="ability in abilities"
      :key="ability.index"
      :ability="ability.contents"
      :data-image="image"
      @click="choose(ability.index)"
      />
    <template v-if="debug.active">
      <button v-if="!asset.owner" @click="debug.send(game.id, {tag: 'TakeControlOfAsset', contents: [investigatorId, id]})">Take control</button>
      <button v-if="asset.owner" @click="debug.send(game.id, {tag: 'Discard', contents: { tag: 'AssetTarget', contents: id}})">Discard</button>
    </template>
    <div v-if="hasPool" class="pool">
      <div class="keys" v-if="keys.length > 0">
        <Key v-for="key in keys" :key="key" :name="key" />
      </div>
      <PoolItem
        v-if="asset.uses && asset.uses.amount > 0"
        type="resource"
        :amount="asset.uses.amount"
      />
      <PoolItem
        v-if="asset.health !== null || (damage || 0) > 0"
        type="health"
        :amount="damage || 0"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="choose(healthAction)"
      />
      <PoolItem
        v-if="asset.sanity !== null || (horror || 0) > 0"
        type="sanity"
        :amount="horror || 0"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="choose(sanityAction)"
      />
      <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
      <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
      <PoolItem v-if="resources && resources > 0" type="resource" :amount="resources" />
      <Token v-for="(sealedToken, index) in asset.sealedChaosTokens" :key="index" :token="sealedToken" :investigatorId="investigatorId" :game="game" @choose="choose" />
    </div>
    <Asset
      v-for="assetId in asset.assets"
      :asset="game.assets[assetId]"
      :game="game"
      :investigatorId="investigatorId"
      :key="assetId"
      @choose="$emit('choose', $event)"
    />
  </div>
</template>

<style lang="scss" scoped>
.card {
  width: $card-width;
  max-width: $card-width;
  border-radius: 5px;
  transform: rotate(0deg);
  transition: transform 0.2s linear;
}

.asset {
  display: flex;
  flex-direction: column;
}

.exhausted {
  transition: transform 0.2s linear;
  transform: rotate(90deg);
  padding: 0 30px;
}

.asset--can-interact {
  border: 2px solid $select;
  cursor:pointer;
}

.pool {
  display: flex;
  flex-direction: row;
  height: 2em;
  justify-content: center;
}

.button{
  margin-top: 2px;
  border: 0;
  color: #fff;
  border-radius: 4px;
  border: 1px solid #ff00ff;
}

:deep(.token) {
  width: 30px;
  height: 30px;
}

:deep(.event img) {
  object-fit: cover;
  object-position: 0 -72px;
  height: 36px;
  margin-top: 2px;
}
</style>
