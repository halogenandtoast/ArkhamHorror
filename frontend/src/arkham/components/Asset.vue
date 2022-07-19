<script lang="ts" setup>
import { computed, inject } from 'vue';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { Message } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';
import Event from '@/arkham/components/Event.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilityButton from '@/arkham/components/AbilityButton.vue'
import Token from '@/arkham/components/Token';
import * as Arkham from '@/arkham/types/Asset';

export interface Props {
  game: Game
  asset: Arkham.Asset
  investigatorId: string
}

const props = defineProps<Props>()
const emit = defineEmits(['showCards', 'choose'])

const id = computed(() => props.asset.id)
const hasPool = computed(() => {
  const {
    sanity,
    health,
    horror,
    uses,
    doom,
    clues,
    sealedTokens,
  } = props.asset;
  return sanity || health || horror || uses || doom > 0 || clues > 0 || sealedTokens.length > 0;
})

const exhausted = computed(() => props.asset.exhausted)
const cardCode = computed(() => props.asset.cardCode)
const image = computed(() => {
  const baseUrl = process.env.NODE_ENV == 'production' ? "https://assets.arkhamhorror.app" : '';
  return `${baseUrl}/img/arkham/cards/${cardCode.value.replace('c', '')}.jpg`
})
const choices = computed(() => ArkhamGame.choices(props.game, props.investigatorId))

function canInteract(c: Message): boolean {
  switch (c.tag) {
    case MessageType.DISCARD:
      return c.contents.contents === id.value
    case MessageType.READY:
      return c.contents.contents === id.value
    case MessageType.FLIP:
      return c.contents[1].contents === id.value
    case MessageType.REMOVE_DOOM:
      return c.contents[0].contents === id.value
    case MessageType.LOOK_AT_REVEALED:
      return c.contents[1].contents === id.value
    case MessageType.ADD_USES:
      return c.contents[0].contents === id.value
    case MessageType.USE_CARD_ABILITY:
      return c.contents[1].contents === id.value
    // case MessageType.ACTIVATE_ABILITY:
    //   return c.contents[1].source.contents === id.value
    //     && (c.contents[1].type.tag === 'ReactionAbility')
    case MessageType.RUN:
      return c.contents.some((c1: Message) => canInteract(c1))
    case MessageType.TARGET_LABEL:
      return c.contents[0].tag === "AssetTarget" && c.contents[0].contents === id.value
    default:
      return false;
  }
}

function canAdjustHealth(c: Message): boolean {
  switch (c.tag) {
    case MessageType.ASSET_DAMAGE:
      return c.contents[0] === id.value && c.contents[2] > 0;
    case MessageType.RUN:
      return c.contents.some((c1: Message) => canAdjustHealth(c1));
    default:
      return false;
  }
}

function canAdjustSanity(c: Message): boolean {
  switch (c.tag) {
    case MessageType.ASSET_DAMAGE:
      return c.contents[0] === id.value && c.contents[3] > 0;
    case MessageType.RUN:
      return c.contents.some((c1: Message) => canAdjustSanity(c1));
    default:
      return false;
  }
}

const cardAction = computed(() => choices.value.findIndex(canInteract))
const healthAction = computed(() => choices.value.findIndex(canAdjustHealth))
const sanityAction = computed(() => choices.value.findIndex(canAdjustSanity))

function isActivate(v: Message) {
  if (v.tag !== 'UseAbility') {
    return false
  }

  const { tag, contents } = v.contents[1].source;

  if (tag === 'AssetSource' && contents === id.value) {
    return true
  }

  if (tag === 'ProxySource' && contents[0].tag === 'AssetSource' && contents[0].contents === id.value) {
    return true
  }

  return false
}

const abilities = computed(() => {
  return choices
    .value
    .reduce<number[]>((acc, v, i) => {
      if (v.tag === 'Run' && isActivate(v.contents[0])) {
        return [...acc, i];
      } else if (isActivate(v)) {
        return [...acc, i];
      }

      return acc;
    }, []);
})

const cardsUnderneath = computed(() => props.asset.cardsUnderneath)
const cardsUnderneathLabel = computed(() => `Underneath (${cardsUnderneath.value.length})`)

const showCardsUnderneath = (e: Event) => emit('showCards', e, cardsUnderneath, "Cards Underneath", false)

const debug = inject('debug')
const debugChoose = inject('debugChoose')

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
      :key="ability"
      :ability="choices[ability]"
      :data-image="image"
      @click="choose(ability)"
      />
    <template v-if="debug">
      <button v-if="!asset.investigator" @click="debugChoose({tag: 'TakeControlOfAsset', contents: [investigatorId, id]})">Take control</button>
      <button v-if="asset.investigator" @click="debugChoose({tag: 'Discard', contents: { tag: 'AssetTarget', contents: id}})">Discard</button>
    </template>
    <div v-if="hasPool" class="pool">
      <PoolItem
        v-if="asset.uses && asset.uses.amount > 0"
        type="resource"
        :amount="asset.uses.amount"
      />
      <PoolItem
        v-if="asset.health !== null"
        type="health"
        :amount="asset.damage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="choose(healthAction)"
      />
      <PoolItem
        v-if="asset.sanity !== null"
        type="sanity"
        :amount="asset.horror"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="choose(sanityAction)"
      />
      <PoolItem v-if="asset.doom > 0" type="doom" :amount="asset.doom" />
      <PoolItem v-if="asset.clues > 0" type="clue" :amount="asset.clues" />
      <Token v-for="(sealedToken, index) in asset.sealedTokens" :key="index" :token="sealedToken" :investigatorId="investigatorId" :game="game" @choose="choose" />
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
}

.asset {
  display: flex;
  flex-direction: column;
}

.exhausted {
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
  width: 20px;
}

:deep(.event) {
  object-fit: cover;
  object-position: 0 -72px;
  height: 68px;
  margin-top: 2px;
}
</style>
