<script lang="ts" setup>

import { useMenu } from '@/composable/menu';
import Draggable from '@/components/Draggable.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import { computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import type { Game } from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Location';
import { cardImg } from '@/arkham/helpers';
import { TokenType, type Token } from '@/arkham/types/Token';

type Props = {
  game: Game
  location: Arkham.Location
  playerId: string
}

const emit = defineEmits<{ close: [] }>()
const props = defineProps<Props>()
const { addEntry } = useMenu()
const placeTokens = ref(false);
const placeTokenType = ref<Token>("Clue");
const tokenTypes = Object.values(TokenType);
const floodLevels: Arkham.FloodLevel[] = ['Unflooded', 'PartiallyFlooded', 'FullyFlooded'];

const isNumber = (value: unknown): value is number => typeof value === 'number';
const anyTokens = computed(() => Object.values(props.location.tokens).some(t => isNumber(t) && t > 0))
const canAdjustFloodLevel = computed(() => {
  const campaignId = props.game.campaign?.id;
  const scenarioId = props.game.scenario?.id.replace(/^c/, '');
  return campaignId === '07' || campaignId === '11' || scenarioId?.startsWith('07') || scenarioId?.startsWith('11');
})
const currentFloodLevel = computed<Arkham.FloodLevel>(() => props.location.floodLevel ?? 'Unflooded')

addEntry({
  id: `close-debug-${props.location.id}`,
  content: "",
  shortcut: "Escape",
  action: () => emit('close')
})


const debug = useDebug()
const id = computed(() => props.location.id)
const cardCode = computed(() => props.location.cardCode)
const image = computed(() => {
  return cardImg(cardCode.value.replace('c', ''))
})

const clues = computed(() => props.location.tokens[TokenType.Clue])

const setFloodLevel = (level: Arkham.FloodLevel) => {
  debug.send(props.game.id, { tag: 'SetFloodLevel', contents: [id.value, level] })
}

const hasPool = computed(() => {
  return (clues.value ?? 0) > 0;
})

const createModifier = (target: {tag: string, contents: string}, modifier: {tag: string, contents: unknown}) => 
  debug.send(props.game.id,
    { tag: 'CreateWindowModifierEffect'
    , contents:
      [ {tag: 'EffectGameWindow'}
      , { tag: 'EffectModifiers'
        , contents:
          [ { source: {tag: 'GameSource'}
            , type: modifier
            , activeDuringSetup: false
            , card: null}
          ]
        }
      , {tag: 'GameSource'}
      , target
      ]
    })

</script>

<template>
  <Draggable>
    <template #handle><h2>{{ $t('debug.location.title') }}</h2></template>
    <div class="location--outer">
      <div class="location" :data-index="location.cardId">
        <div class="card-frame">
          <div class="card-wrapper">
            <img :src="image" class="card-no-overlay" />
          </div>
          <div v-if="hasPool" class="pool">
            <PoolItem v-if="(clues ?? 0) > 0" type="clue" :amount="clues ?? 0" />
          </div>
        </div>
      </div>
      <div v-if="placeTokens" class="buttons">
        <select v-model="placeTokenType">
          <option v-for="token in tokenTypes" :key="token" :value="token">{{ token }}</option>
        </select>
        <button @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{ tag: 'GameSource' }, { tag: 'LocationTarget', contents: id}, placeTokenType, 1]}})">{{ $t('debug.common.place') }}</button>
        <button @click="placeTokens = false">{{ $t('debug.common.back') }}</button>
      </div>
      <div v-else class="buttons">
        <div v-if="canAdjustFloodLevel" class="flood-level-controls">
          <span>{{ $t('debug.location.floodLevel') }}</span>
          <button
            v-for="level in floodLevels"
            :key="level"
            :disabled="level === currentFloodLevel"
            @click="setFloodLevel(level)"
          >
            {{ $t(`debug.location.floodLevels.${level}`) }}
          </button>
        </div>
        <button v-if="location.cardCode == 'c03139'" @click="createModifier({tag: 'LocationTarget', contents: id}, {tag: 'AddTrait', contents: 'Passageway'})">{{ $t('debug.location.addPassageway') }}</button>
        <button v-if="!location.revealed" @click="debug.send(game.id, {tag: 'RevealLocation', contents: [null, id]})">{{ $t('debug.location.reveal') }}</button>
        <button v-if="clues && clues > 0" @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'RemoveTokens_', contents: [{ tag: 'TestSource', contents: []}, { tag: 'LocationTarget', contents: id }, 'Clue', clues]}})">{{ $t('debug.location.removeClues') }}</button>
        <button @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{ tag: 'TestSource', contents: []}, { tag: 'LocationTarget', contents: id }, 'Clue', 1]}})">{{ $t('debug.location.placeClue') }}</button>
        <button v-if="location.revealed" @click="debug.send(game.id, {tag: 'Reset', contents: { 'tag': 'LocationTarget', contents: id }})">{{ $t('debug.location.reset') }}</button>
        <button @click="placeTokens = true">{{ $t('debug.common.placeTokens') }}</button>
        <button v-if="anyTokens" @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'ClearTokens_', contents: { tag: 'LocationTarget', contents: id}}})">{{ $t('debug.common.removeAllTokens') }}</button>
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

.location {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.buttons {
  display: flex;
  flex-direction: column;
  justify-content: space-around;
  flex: 1;
  gap: 5px;
}

.flood-level-controls {
  display: flex;
  flex-direction: column;
  gap: 5px;
  padding-bottom: 5px;
  border-bottom: 1px solid var(--border-color, #777);
}

.flood-level-controls span {
  font-weight: bold;
}

.location--outer {
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 10px;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.pool {
  position: absolute;
  top: 40%;
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
</style>
