<script lang="ts" setup>

import { useEscape } from '@/composable/escape';
import Draggable from '@/components/Draggable.vue';
import PoolItem from '@/arkham/components/PoolItem.vue';
import { computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import type { Game } from '@/arkham/types/Game';
import * as Arkham from '@/arkham/types/Location';
import { cardImg } from '@/arkham/helpers';
import { TokenType, type Token } from '@/arkham/types/Token';
import officialCampaigns from '@/arkham/data/campaigns.json';
import allScenarios from '@/arkham/data/scenarios';
import { homebrewCampaigns } from '@/arkham/homebrewData';
import { campaignHasFloodRules, type Campaign } from '@/arkham/data';

type Props = {
  game: Game
  location: Arkham.Location
  playerId: string
}

const emit = defineEmits<{ close: [] }>()
const props = defineProps<Props>()
const placeTokens = ref(false);
const placeTokenType = ref<Token>("Clue");
const tokenTypes = Object.values(TokenType);
const floodLevels: Arkham.FloodLevel[] = ['Unflooded', 'PartiallyFlooded', 'FullyFlooded'];

const isNumber = (value: unknown): value is number => typeof value === 'number';
const anyTokens = computed(() => Object.values(props.location.tokens).some(t => isNumber(t) && t > 0))

/* The flood controls follow the campaign's declared `floodRules` rather than an id
   allowlist, which had hidden them from the homebrew Return to Innsmouth box. A
   standalone has no campaign in the game, so the scenario entry names its campaign. */
const allCampaigns = [...(officialCampaigns as Campaign[]), ...homebrewCampaigns]
const canAdjustFloodLevel = computed(() => {
  const scenarioId = props.game.scenario?.id.replace(/^c/, '')
  const campaignId =
    props.game.campaign?.id ?? allScenarios.find((s) => s.id === scenarioId)?.campaign
  return campaignHasFloodRules(allCampaigns.find((c) => c.id === campaignId))
})
const currentFloodLevel = computed<Arkham.FloodLevel>(() => props.location.floodLevel ?? 'Unflooded')

useEscape(() => emit('close'))

const debug = useDebug()
const id = computed(() => props.location.id)
const cardCode = computed(() => props.location.cardCode)
const image = computed(() => {
  return cardImg(cardCode.value.replace(/^c/, ''))
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
    <div class="debug-modal debug-window">
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
      </div>
      </div>
      <button class="debug-close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
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
  display: flex;
  flex-direction: row;
  /* Card pinned to the top: the button column is taller than the art, and
     centring it left the card floating mid-panel. */
  align-items: flex-start;
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
