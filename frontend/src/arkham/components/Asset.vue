<script lang="ts" setup>
import { ComputedRef, computed, watch, ref } from 'vue';
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
  playerId: string
}>()

const emits = defineEmits<{
  choose: [value: number]
  showCards: [e: Event, cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
}>()

const investigatorId = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId)?.id)
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
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function isCardAction(c: Message): boolean {
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

const cardAction = computed(() => choices.value.findIndex(isCardAction))
const canInteract = computed(() => abilities.value.length > 0 || cardAction.value !== -1)
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

const showCardsUnderneath = (e: Event) => emits('showCards', e, cardsUnderneath, "Cards Underneath", false)

const keys = computed(() => props.asset.keys)

const debug = useDebug()

const doom = computed(() => props.asset.tokens[TokenType.Doom])
const clues = computed(() => props.asset.tokens[TokenType.Clue])
const resources = computed(() => props.asset.tokens[TokenType.Resource])

const damage = computed(() => props.asset.tokens[TokenType.Damage])
const horror = computed(() => props.asset.tokens[TokenType.Horror])

const choose = (idx: number) => emits('choose', idx)

const showAbilities = ref<boolean>(false)

async function clicked() {
  if(cardAction.value !== -1) {
    emits('choose', cardAction.value)
  } else if (abilities.value.length > 0) {
    showAbilities.value = !showAbilities.value
  }
}

async function chooseAbility(ability: number) {
  showAbilities.value = false
  emits('choose', ability)
}

watch(abilities, (abilities) => {
  // ability is forced we must show
  if (abilities.some(a => "ability" in a.contents && a.contents.ability.type.tag === "ForcedAbility")) {
    showAbilities.value = true
  }

  if (abilities.length === 0) {
    showAbilities.value = false
  }
})
</script>

<template>
  <div class="asset" :data-index="asset.cardId">
    <div class="card-frame">
      <img
        :data-id="id"
        :src="image"
        :class="{ 'asset--can-interact': canInteract, exhausted}"
        class="card"
        @click="clicked"
      />
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
        <Token v-for="(sealedToken, index) in asset.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" />
      </div>

      <div v-if="showAbilities" class="abilities" :data-image="image">
        <AbilityButton
          v-for="ability in abilities"
          :key="ability.index"
          :ability="ability.contents"
          @click="chooseAbility(ability.index)"
          />
      </div>
    </div>
    <Event
      v-for="eventId in asset.events"
      :event="game.events[eventId]"
      :game="game"
      :playerId="playerId"
      :key="eventId"
      @choose="$emit('choose', $event)"
    />
    <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
    <template v-if="debug.active">
      <button v-if="!asset.owner" @click="debug.send(game.id, {tag: 'TakeControlOfAsset', contents: [investigatorId, id]})">Take control</button>
      <button v-if="asset.owner" @click="debug.send(game.id, {tag: 'Discard', contents: [{ tag: 'GameSource' }, { tag: 'AssetTarget', contents: id}]})">Discard</button>
    </template>
    <Asset
      v-for="assetId in asset.assets"
      :asset="game.assets[assetId]"
      :game="game"
      :playerId="playerId"
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
  position: absolute;
  top: 50%;
  align-items: center;
  display: flex;
  * {
    transform: scale(0.6);
  }

  pointer-events: none;
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

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.abilities {
  position: absolute;
  padding: 10px;
  background: rgba(0, 0, 0, 0.8);
  border-radius: 10px;
  display: flex;
  flex-direction: column;
  gap: 5px;
  bottom:100%;
  left: 0;
}
</style>
