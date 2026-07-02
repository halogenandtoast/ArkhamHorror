<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue';
import { computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import { TokenType, Token } from '@/arkham/types/Token';
import { imgsrc } from '@/arkham/helpers';
import { cardImage as cardToImage, asCardCode, toCardContents, type Card as ArkhamCard } from '@/arkham/types/Card';
import { keyToId } from '@/arkham/types/Key'
import type { Game } from '@/arkham/types/Game';
import KeyToken from '@/arkham/components/Key.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import TokenView from '@/arkham/components/Token.vue';
import * as Arkham from '@/arkham/types/Asset';
import {isUse} from '@/arkham/types/Token';
import { useDbCardStore } from '@/stores/dbCards'
import { useCardStore } from '@/stores/cards'

const props = defineProps<{
  game: Game
  asset: Arkham.Asset
  playerId: string
}>()

const emit = defineEmits<{ close: [] }>()
const placeTokens = ref(false);
const setModifiers = ref(false);
const inspectSpiritDeck = ref(false);
const placeTokenType = ref<Token>(
  (Object.
    entries(props.asset.tokens).
    filter(([k, v]) => isUse(k) && (v ?? 0) > 0)[0] ?? [])[0]
  ?? "Evidence"
);

const isNumber = (value: unknown): value is number => typeof value === 'number';
const anyTokens = computed(() => Object.values(props.asset.tokens).some(t => isNumber(t) && t > 0))

const investigatorId = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId)?.id)
const id = computed(() => props.asset.id)
// Flip needs an investigator (disciplines re-place themselves via ReplaceInvestigatorAsset);
// prefer the asset's owner, fall back to the debugging player's investigator.
const flipInvestigator = computed(() => props.asset.owner ?? investigatorId.value)
// An asset can flip when any card def declares an otherSide link to/from its code
// (disciplines declare both sides; Sophie's back side points to her front).
const cardStore = useCardStore()
const canFlip = computed(() => {
  const code = cardCode.value.replace(/^c/, '')
  return cardStore.cards.some(c => (c.cardCode === code && c.otherSide !== null) || c.otherSide === code)
})

const exhausted = computed(() => props.asset.exhausted)
const cardCode = computed(() => props.asset.cardCode)
const image = computed(() => {
  return imgsrc(`cards/${cardCode.value.replace('c', '')}.avif`)
})

const keys = computed(() => props.asset.keys)
const debug = useDebug()
const dbCardStore = useDbCardStore()
const doom = computed(() => props.asset.tokens[TokenType.Doom])
const clues = computed(() => props.asset.tokens[TokenType.Clue])
const uses = computed(() => Object.entries(props.asset.tokens).filter(([k, v]) => isUse(k) && (v ?? 0) > 0))
const formatUse = (k: string) => k.replace(/([a-z])([A-Z])/g, '$1 $2')
const damage = computed(() => props.asset.tokens[TokenType.Damage])
const horror = computed(() => props.asset.tokens[TokenType.Horror])

const tokenTypes = Object.values(TokenType);
const spiritDeck = computed(() => props.asset.spiritDeck ?? [])
const spiritDeckCardImage = (card: ArkhamCard) => imgsrc(cardToImage(card))
const spiritDeckCardImageId = (card: ArkhamCard) => toCardContents(card).art ?? asCardCode(card).replace(/^c/, '')
const spiritDeckCardName = (card: ArkhamCard) => {
  const contents = toCardContents(card)
  const dbCard = dbCardStore.getDbCard(contents.art ?? asCardCode(card).replace(/^c/, ''))
  if (!dbCard) return 'Unknown card'
  return dbCard.subname ? `${dbCard.name}: ${dbCard.subname}` : dbCard.name
}

const showSlots = ref(false)
const slots = computed(() => {
  const {owner} = props.asset
  if (!owner) return []
  const investigator = Object.values(props.game.investigators).find(i => i.id === owner)
  if (!investigator) return []
  return investigator.slots.filter((s) => s.assets.includes(props.asset.id))
});

const hasPool = computed(() => {
  const {
    sanity,
    health,
    tokens,
    sealedChaosTokens,
    keys,
  } = props.asset;

  return cardCode.value == 'c07189' || (Object.values(tokens).some((v) => (v ?? 0) > 0) || sealedChaosTokens.length > 0 || keys.length > 0 || sanity || health)
})
</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.asset.title') }}</h2></template>
    <div class="asset--outer">
      <div class="asset" :data-index="asset.cardId">
        <div class="card-frame">
          <div class="card-wrapper">
            <img :src="image" class="card-no-overlay" />
          </div>
          <div v-if="hasPool" class="pool">
            <div class="keys" v-if="keys.length > 0">
              <KeyToken v-for="k in keys" :key="keyToId(k)" :keyToken="k" :game="game" :playerId="playerId" />
            </div>
            <template v-for="[use, amount] in uses" :key="use">
              <PoolItem
                v-if="(amount ?? 0) > 0"
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
        <button @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{ tag: 'GameSource' }, { tag: 'AssetTarget', contents: id}, placeTokenType, 1]}})">{{ $t('debug.common.place') }}</button>
      </div>
      <div v-if="showSlots" class="buttons">
        <div class="slots">{{slots}}</div>
        <button @click="showSlots = false">{{ $t('debug.common.back') }}</button>
      </div>
      <div v-else-if="inspectSpiritDeck" class="spirit-deck-inspector">
        <div class="spirit-deck-inspector__header">Spirit Deck ({{ spiritDeck.length }})</div>
        <div v-if="spiritDeck.length > 0" class="spirit-deck-inspector__cards">
          <div v-for="(card, index) in spiritDeck" :key="`${toCardContents(card).id}-${index}`" class="spirit-deck-inspector__card">
            <span class="spirit-deck-inspector__position">{{ index + 1 }}</span>
            <img
              class="spirit-deck-inspector__image"
              :src="spiritDeckCardImage(card)"
              :data-image-id="spiritDeckCardImageId(card)"
            />
            <span class="spirit-deck-inspector__name">{{ spiritDeckCardName(card) }}</span>
          </div>
        </div>
        <div v-else class="spirit-deck-inspector__empty">The spirit deck is empty.</div>
        <button @click="inspectSpiritDeck = false">{{ $t('debug.common.back') }}</button>
      </div>
      <div v-else-if="setModifiers" class="buttons">
        <button @click="setModifiers = false">{{ $t('debug.common.back') }}</button>
        <Modifier :modifier="modifier" v-for="(modifier, idx) in asset.modifiers" :key="idx" />
      </div>
      <div v-else class="buttons">
        <button @click="placeTokens = true">{{ $t('debug.common.placeTokens') }}</button>
        <button v-if="anyTokens" @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'ClearTokens_', contents: { tag: 'AssetTarget', contents: id}}})">{{ $t('debug.common.removeAllTokens') }}</button>
        <button v-if="asset.owner !== investigatorId" @click="debug.send(game.id, {tag: 'TakeControlOfAsset', contents: [investigatorId, id]})">{{ $t('debug.asset.takeControl') }}</button>
        <button v-if="exhausted" @click="debug.send(game.id, {tag: 'Ready', contents: { tag: 'AssetTarget', contents: id}})">{{ $t('debug.asset.ready') }}</button>
        <button v-else @click="debug.send(game.id, {tag: 'Exhaust', contents: { tag: 'AssetTarget', contents: id}})">{{ $t('debug.asset.exhaust') }}</button>
        <button v-if="asset.health || asset.sanity" @click="debug.send(game.id, {tag: 'AssetDefeated', contents: [{ tag: 'GameSource' }, id]})">{{ $t('debug.asset.defeat') }}</button>
        <button v-if="asset.owner" @click="debug.send(game.id, {tag: 'Discard', contents: [null, { tag: 'GameSource' }, { tag: 'AssetTarget', contents: id}]})">{{ $t('debug.asset.discard') }}</button>
        <button v-if="canFlip" @click="debug.send(game.id, {tag: 'Flip', contents: [flipInvestigator, { tag: 'GameSource' }, { tag: 'AssetTarget', contents: id}]})">{{ $t('debug.asset.flip') }}</button>
        <button v-if="asset.spiritDeck" @click="inspectSpiritDeck = true">Inspect spirit deck</button>
        <button v-if="slots.length > 0" @click="showSlots = true">{{ $t('debug.asset.showSlots') }}</button>
        <button @click="setModifiers = true">{{ $t('debug.common.modifiers') }}</button>
        <button @click="emit('close')">{{ $t('debug.common.close') }}</button>
      </div>

    </div>
  </Draggable>
</template>

<style scoped>
.card-no-overlay {
  width: calc(var(--card-width) * 5); 
  max-width: calc(var(--card-width) * 5);
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
  flex: 1;
  gap: 5px;
}

.asset--outer {
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 10px;
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
  border: 1px solid var(--select);
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
  z-index: var(--z-index-1000);
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

.slots {
  font-size: 0.8em;
  color: #ccc;
}

.spirit-deck-inspector {
  display: flex;
  flex-direction: column;
  gap: 8px;
  width: min(520px, 60vw);
  max-height: 70vh;
  color: #ddd;
}

.spirit-deck-inspector__header {
  font-weight: bold;
  color: white;
}

.spirit-deck-inspector__cards {
  display: flex;
  flex-direction: column;
  gap: 6px;
  overflow-y: auto;
  padding-right: 4px;
}

.spirit-deck-inspector__card {
  display: grid;
  grid-template-columns: 2ch 48px 1fr;
  align-items: center;
  gap: 8px;
  padding: 4px;
  border-radius: 4px;
  background: rgba(255, 255, 255, 0.08);
}

.spirit-deck-inspector__position {
  color: #aaa;
  text-align: right;
}

.spirit-deck-inspector__image {
  width: 48px;
  border-radius: 3px;
}

.spirit-deck-inspector__name,
.spirit-deck-inspector__empty {
  font-size: 0.9em;
}
</style>
