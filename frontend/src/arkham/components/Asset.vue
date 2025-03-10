<script lang="ts" setup>
import { ComputedRef, computed, watch, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import { TokenType } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import * as ArkhamGame from '@/arkham/types/Game';
import type { AbilityLabel, AbilityMessage, Message } from '@/arkham/types/Message';
import type { AbilityType } from '@/arkham/types/Ability';
import { MessageType } from '@/arkham/types/Message';
import DebugAsset from '@/arkham/components/debug/Asset.vue';
import Key from '@/arkham/components/Key.vue';
import Investigator from '@/arkham/components/Investigator.vue';
import Event from '@/arkham/components/Event.vue';
import Enemy from '@/arkham/components/Enemy.vue';
import Treachery from '@/arkham/components/Treachery.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import AbilitiesMenu from '@/arkham/components/AbilitiesMenu.vue'
import Story from '@/arkham/components/Story.vue';
import Token from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Asset';
import {isUse} from '@/arkham/types/Token';
import { Card } from '../types/Card';

const props = withDefaults(defineProps<{
  game: Game
  asset: Arkham.Asset
  playerId: string
  atLocation?: boolean
}>(), { atLocation: false })

const debugging = ref(false)
const frame = ref(null)

const emits = defineEmits<{
  choose: [value: number]
  showCards: [e: Event, cards: ComputedRef<Card[]>, title: string, isDiscards: boolean]
}>()

const id = computed(() => props.asset.id)
const exhausted = computed(() => props.asset.exhausted)
const cardCode = computed(() => props.asset.cardCode)
const investigators = computed(() => Object.values(props.game.investigators).filter((i) => i.placement.tag === 'InVehicle' && i.placement.contents === id.value))
const image = computed(() => {
  const mutated = props.asset.mutated ? `_${props.asset.mutated}` : ''
  if (props.asset.flipped) {
    if (cardCode.value === "c90052") {
      return imgsrc(`cards/90052b.avif`)
    }
    return imgsrc(`player_back.jpg`)
  }
  return imgsrc(`cards/${cardCode.value.replace('c', '')}${mutated}.avif`)
})

const dataImage = computed(() => {
  const mutated = props.asset.mutated ? `_${props.asset.mutated}` : ''
  if (props.asset.flipped) {
    if (cardCode.value === "c90052") {
      return "90052b"
    }
  }
  return cardCode.value.replace('c', '') + mutated
})
const choices = computed(() => ArkhamGame.choices(props.game, props.playerId))

function isCardAction(c: Message): boolean {
  if (c.tag === MessageType.TARGET_LABEL) {
    return c.target.contents === id.value || c.target.contents === props.asset.cardId
      || `c${id.value}` === c.target.contents

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

const isSpirit = computed(() => (props.asset.modifiers ?? []).some((m) => m.type.contents === 'IsSpirit'))

function isAbility(v: Message): v is AbilityLabel {
  if (v.tag !== MessageType.ABILITY_LABEL) {
    return false
  }

  const { source } = v.ability;

  if (source.sourceTag === 'ProxySource') {
    if (source.source.tag === 'CardCodeSource') {
      return source.originalSource.contents === id.value
    }
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
        return [...acc, { contents: v, displayAsAction: false, index: i }];
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
const uses = computed(() => Object.entries(props.asset.tokens).filter(([k, v]) => isUse(k) && v > 0))
const formatUse = (k: string) => k.replace(/([a-z])([A-Z])/g, '$1 $2')

const damage = computed(() => props.asset.tokens[TokenType.Damage])
const horror = computed(() => props.asset.tokens[TokenType.Horror])

const hasPool = computed(() => {
  const {
    sanity,
    health,
    tokens,
    sealedChaosTokens,
    keys,
  } = props.asset;

  return cardCode.value == 'c07189' || (Object.values(tokens).some((v) => v > 0) || sealedChaosTokens.length > 0 || keys.length > 0 || sanity || health)
})

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
  let isForced = (type: AbilityType) => {
    switch (type.tag) {
      case "ForcedAbility": return true
      case "DelayedAbility": return isForced(type.abilityType)
      default: return false
    }
  }
  if (abilities.some(a => "ability" in a.contents && isForced(a.contents.ability.type))) {
    showAbilities.value = true
  }

  if (abilities.length === 0) {
    showAbilities.value = false
  }
})

const assetStory = computed(() => {
  const { stories } = props.game
  return Object.values(stories).find((s) => s.otherSide?.contents === props.asset.id)
})
</script>

<template>
  <div class="asset--outer">
    <Story v-if="assetStory" :story="assetStory" :game="game" :playerId="playerId" @choose="choose"/>
    <div v-else class="asset" :data-index="asset.cardId">
      <div class="card-frame" ref="frame">
        <div v-if="asset.marketDeck" class="market-deck">
          <img
            class="deck card"
            :src="imgsrc('player_back.jpg')"
            width="150px"
          />
          <span class="deck-size">{{asset.marketDeck.length}}</span>
        </div>
        <div v-if="asset.spiritDeck" class="spirit-deck">
          <img
            class="deck card"
            :src="imgsrc('player_back.jpg')"
            width="150px"
          />
          <span class="deck-size">{{asset.spiritDeck.length}}</span>
        </div>
        <div class="card-wrapper" :class="{ 'asset--can-interact': canInteract}">
          <img
            :data-id="id"
            :data-image-id="dataImage"
            :src="image"
            class="card"
            :class="{ exhausted }"
            @click="clicked"
            :data-customizations="JSON.stringify(asset.customizations)"
          />
          <div v-if="investigators.length > 0" class="in-vehicle">
            <div v-for="investigator in investigators" :key="investigator.id">
              <Investigator
                :game="game"
                :choices="choices"
                :playerId="playerId"
                :portrait="true"
                :investigator="investigator"
                @choose="$emit('choose', $event)"
                />
            </div>
          </div>
        </div>
        <div v-if="hasPool" class="pool">
          <div class="keys" v-if="keys.length > 0">
            <Key v-for="key in keys" :key="key" :name="key" />
          </div>
          <template v-for="[use, amount] in uses" :key="use">
            <PoolItem
              v-if="amount > 0"
              type="resource"
              :tooltip="formatUse(use)"
              :amount="amount"
            />
          </template>
          <PoolItem
            v-if="!isSpirit && (cardCode == 'c07189' || (asset.health !== null || (damage || 0) > 0))"
            type="health"
            :amount="damage || 0"
            :class="{ 'health--can-interact': healthAction !== -1 }"
            @choose="choose(healthAction)"
          />
          <PoolItem
            v-if="!isSpirit && (cardCode == 'c07189' || (asset.sanity !== null || (horror || 0) > 0))"
            type="sanity"
            :amount="horror || 0"
            :class="{ 'sanity--can-interact': sanityAction !== -1 }"
            @choose="choose(sanityAction)"
          />
          <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
          <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
          <Token v-for="(sealedToken, index) in asset.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" @choose="choose" />
        </div>
        <AbilitiesMenu
          v-model="showAbilities"
          :frame="frame"
          :abilities="abilities"
          :game="game"
          @choose="chooseAbility"
        />
      </div>
      <Event
        v-for="eventId in asset.events"
        :event="game.events[eventId]"
        :game="game"
        :playerId="playerId"
        :key="eventId"
        :attached="true"
        @choose="$emit('choose', $event)"
      />
      <Treachery
        v-for="treacheryId in asset.treacheries"
        :treachery="game.treacheries[treacheryId]"
        :game="game"
        :attached="true"
        :playerId="playerId"
        :key="treacheryId"
        @choose="$emit('choose', $event)"
      />
      <button v-if="cardsUnderneath.length > 0" class="view-discard-button" @click="showCardsUnderneath">{{cardsUnderneathLabel}}</button>
      <template v-if="debug.active">
        <button @click="debugging = true">Debug</button>
      </template>
      <Asset
        v-for="assetId in asset.assets"
        :asset="game.assets[assetId]"
        :game="game"
        :playerId="playerId"
        :key="assetId"
        @choose="$emit('choose', $event)"
      />
      <Enemy
        v-for="enemyId in asset.enemies"
        :enemy="game.enemies[enemyId]"
        :game="game"
        :playerId="playerId"
        :key="enemyId"
        @choose="$emit('choose', $event)"
      />
    </div>
    <DebugAsset v-if="debugging" :game="game" :asset="asset" :playerId="playerId" @close="debugging = false" @choose="$emit('choose', $event)"/>
  </div>
</template>

<style lang="scss" scoped>
.card {
  width: var(--card-width);
  max-width: var(--card-width);
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
  margin: 0 30px;
}

.asset--can-interact {
  img {
    border: 2px solid var(--select);
    cursor:pointer;
  }
}

.pool {
  position: absolute;
  top: 50%;
  align-items: center;
  width: 100%;
  display: flex;
  flex-wrap: wrap;
  :deep(.token-container) {
    width: unset;
  }
  :deep(img) {
    width: 20px;
    height: auto;
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
  object-position: bottom;
  height: 36px;
  margin-top: 2px;
}

:deep(.event .exhausted) {
  padding: 0px;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.deck-size {
  pointer-events: none;
  position: absolute;
  font-weight: bold;
  font-size: 1.2em;
  color: rgba(255, 255, 255, 0.6);
  left: 50%;
  top: 40%;
  background: rgba(0, 0, 0, 0.6);
  padding: 10px;
  border-radius: 20px;
  transform: translateX(-50%) translateY(-50%);
}

.market-deck {
  position: relative;
  margin-right: 5px;
}

.spirit-deck {
  position: relative;
  margin-right: 5px;
}

.in-vehicle {
  position: absolute;
  top: 2px;
  left: 2px;
  width: 100%;
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: 1fr 1fr;
  pointer-events: none;
  gap: 4px;

  .vehicle-investigator {
    line-height: 0;
    height: fit-content;
  }

  &:deep(.portrait) {
    pointer-events: auto;
    width: 100%;
  }
}
</style>
