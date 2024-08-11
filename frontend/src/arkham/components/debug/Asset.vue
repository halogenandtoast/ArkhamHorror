<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue';
import { computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import { TokenType, Token } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import Key from '@/arkham/components/Key.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import TokenView from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Asset';
import {isUse} from '@/arkham/types/Token';

const props = defineProps<{
  game: Game
  asset: Arkham.Asset
  playerId: string
}>()

const emit = defineEmits<{ close: [] }>()
const placeTokens = ref(false);
const placeTokenType = ref<Token>("Evidence");

const isNumber = (value: unknown): value is number => typeof value === 'number';
const anyTokens = computed(() => Object.values(props.asset.tokens).some(t => isNumber(t) && t > 0))

const investigatorId = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId)?.id)
const id = computed(() => props.asset.id)

const exhausted = computed(() => props.asset.exhausted)
const cardCode = computed(() => props.asset.cardCode)
const image = computed(() => {
  return imgsrc(`cards/${cardCode.value.replace('c', '')}.jpg`)
})

const keys = computed(() => props.asset.keys)
const debug = useDebug()
const doom = computed(() => props.asset.tokens[TokenType.Doom])
const clues = computed(() => props.asset.tokens[TokenType.Clue])
const uses = computed(() => Object.entries(props.asset.tokens).filter(([k, v]) => isUse(k) && v > 0))
const formatUse = (k: string) => k.replace(/([a-z])([A-Z])/g, '$1 $2')
const damage = computed(() => props.asset.tokens[TokenType.Damage])
const horror = computed(() => props.asset.tokens[TokenType.Horror])

const tokenTypes = Object.values(TokenType);

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
</script>

<template>
  <Draggable>
    <template #handle><h2>Debug Asset</h2></template>
    <div class="asset--outer">
      <div class="asset" :data-index="asset.cardId">
        <div class="card-frame">
          <div class="card-wrapper">
            <img :src="image" class="card-no-overlay" />
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
              v-if="cardCode == 'c07189' || (asset.health !== null || (damage || 0) > 0)"
              type="health"
              :amount="damage || 0"
            />
            <PoolItem
              v-if="cardCode == 'c07189' || (asset.sanity !== null || (horror || 0) > 0)"
              type="sanity"
              :amount="horror || 0"
            />
            <PoolItem v-if="doom && doom > 0" type="doom" :amount="doom" />
            <PoolItem v-if="clues && clues > 0" type="clue" :amount="clues" />
            <TokenView v-for="(sealedToken, index) in asset.sealedChaosTokens" :key="index" :token="sealedToken" :playerId="playerId" :game="game" />
          </div>
        </div>
      </div>
      <div v-if="placeTokens" class="buttons">
        <select v-model="placeTokenType">
          <option v-for="token in tokenTypes" :key="token" :value="token">{{ token }}</option>
        </select>
        <button @click="debug.send(game.id, {tag: 'PlaceTokens', contents: [{ tag: 'GameSource' }, { tag: 'AssetTarget', contents: id}, placeTokenType, 1]})">Place</button>
        <button @click="placeTokens = false">Back</button>
      </div>
      <div v-else class="buttons">
        <button @click="placeTokens = true">Place Tokens</button>
        <button v-if="anyTokens" @click="debug.send(game.id, {tag: 'ClearTokens', contents: { tag: 'AssetTarget', contents: id}})">Remove All Tokens</button>
        <button v-if="!asset.owner" @click="debug.send(game.id, {tag: 'TakeControlOfAsset', contents: [investigatorId, id]})">Take control</button>
        <button v-if="exhausted" @click="debug.send(game.id, {tag: 'Ready', contents: { tag: 'AssetTarget', contents: id}})">Ready</button>
        <button v-else @click="debug.send(game.id, {tag: 'Exhaust', contents: { tag: 'AssetTarget', contents: id}})">Exhaust</button>
        <button v-if="asset.owner" @click="debug.send(game.id, {tag: 'Discard', contents: [null, { tag: 'GameSource' }, { tag: 'AssetTarget', contents: id}]})">Discard</button>
        <button @click="emit('close')">Close</button>
      </div>
    </div>
  </Draggable>
</template>

<style lang="scss" scoped>
.card-no-overlay {
  width: $card-width * 5; 
  max-width: $card-width * 5;
  border-radius: 15px;
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

.buttons {
  display: flex;
  flex-direction: column;
  justify-content: space-around;
}

.asset--outer {
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
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
  z-index: 1000;
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
</style>
