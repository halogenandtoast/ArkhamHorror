<script lang="ts" setup>
import Draggable from '@/components/Draggable.vue';
import { computed, ref } from 'vue';
import { useDebug } from '@/arkham/debug';
import { TokenType, Token } from '@/arkham/types/Token';
import { cardImg } from '@/arkham/helpers';
import type { Game } from '@/arkham/types/Game';
import PoolItem from '@/arkham/components/PoolItem.vue';
import Modifier from '@/arkham/components/Modifier.vue';
import { type ChaosToken, chaosTokenImage } from '@/arkham/types/ChaosToken';
import * as Arkham from '@/arkham/types/Enemy'

const props = defineProps<{
  game: Game
  enemy: Arkham.Enemy
  playerId: string
}>()

const emit = defineEmits<{ close: [] }>()
const placeTokenType = ref<Token>("Damage");
const tokenTypes = Object.values(TokenType);

const isNumber = (value: unknown): value is number => typeof value === 'number';
const anyTokens = computed(() => Object.values(props.enemy.tokens).some(t => isNumber(t) && t > 0))
const isTrueForm = computed(() => {
  const { cardCode } = props.enemy
  return cardCode === 'cxnyarlathotep'
})

function mapMaybe<T, U>(arr: T[], fn: (item: T) => U | null | undefined): U[] {
  return arr.reduce((acc: U[], item: T) => {
    const result = fn(item);
    if (result !== null && result !== undefined) {
      acc.push(result);
    }
    return acc;
  }, []);
}

const addedKeywords = computed(() => {
  const {modifiers} = props.enemy
  return mapMaybe(modifiers, modifier => modifier.type.tag === "AddKeyword" ? modifier.type.contents : null).join(". ")
})

const gainedVictory = computed(() => {
  const {modifiers} = props.enemy

  return modifiers.reduce((acc, modifier) =>
    acc + (modifier.type.tag === "GainVictory" ? modifier.type.contents : 0)
  , 0)
})

const health = computed(() => {
  return props.enemy.health?.tag == "Fixed" ? props.enemy.health.contents : null
})


const investigatorId = computed(() => Object.values(props.game.investigators).find(i => i.playerId === props.playerId)?.id)
const id = computed(() => props.enemy.id)

const cardCode = computed(() => props.enemy.cardCode)
const image = computed(() => {
  return cardImg(cardCode.value.replace(/^c/, ''))
})

const debug = useDebug()
const damage = computed(() => props.enemy.tokens[TokenType.Damage])

const sealedTokens = computed(() => props.enemy.sealedChaosTokens)

/* `UnsealChaosToken` takes the whole record, and `Eq ChaosToken` compares ids, so
 * the other fields only have to be well-formed. Same shape Key.vue sends. */
function unseal(token: ChaosToken) {
  debug.send(props.game.id, {
    tag: 'UnsealChaosToken',
    contents: {
      chaosTokenId: token.id,
      chaosTokenFace: token.face,
      chaosTokenRevealedBy: null,
      chaosTokenCancelled: false,
      chaosTokenSealed: true,
    },
  })
}

function defeat() {
  emit('close')
  debug.send(props.game.id, {
    tag: 'DefeatEnemy',
    contents: [id.value, investigatorId.value, { tag: 'InvestigatorSource', contents: investigatorId.value }],
  })
}

const hasPool = computed(() => {
  const { health } = props.enemy;
  return cardCode.value == 'c07189' || health
})

const createModifier = (target: {tag: string, contents: string}, modifier: {tag: string, contents: unknown}) => 
  debug.send(props.game.id,
    { tag: 'CreateWindowModifierEffect'
    , contents:
      [ {tag: 'EffectGameWindow' }
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
    <template #handle><h2>{{ $t('debug.enemy.title') }}</h2></template>
    <div class="enemy--outer">
      <div class="enemy" :data-index="enemy.cardId">
        <div class="card-frame">
          <div class="card-wrapper">
            <img v-if="isTrueForm" :src="image"
              class="enemy card-no-overlay"
              :data-fight="enemy.fight"
              :data-evade="enemy.evade"
              :data-health="health"
              :data-damage="enemy.healthDamage"
              :data-horror="enemy.sanityDamage"
              :data-victory="gainedVictory"
              :data-keywords="addedKeywords"
            />
            <img v-else :src="image" class="enemy card-no-overlay"
            />
          </div>
          <div v-if="hasPool" class="pool">
            <PoolItem
              v-if="cardCode == 'c07189' || (enemy.health !== null || (damage || 0) > 0)"
              type="health"
              :amount="damage || 0"
            />
          </div>
        </div>
      </div>
      <div class="debug-panel">
        <section class="debug-section">
          <span class="debug-label">{{ $t('debug.enemy.state') }}</span>
          <div class="debug-row">
            <button v-if="!enemy.exhausted" @click="debug.send(game.id, {tag: 'Exhaust', contents: {tag: 'EnemyTarget', contents: id}})">{{ $t('debug.enemy.exhaust') }}</button>
            <button v-else @click="debug.send(game.id, {tag: 'Ready', contents: {tag: 'EnemyTarget', contents: id}})">{{ $t('debug.enemy.ready') }}</button>
            <button @click="debug.send(game.id, {tag: 'EnemyEvaded', contents: [investigatorId, id]})">{{ $t('debug.enemy.evade') }}</button>
            <button class="debug-button--danger" @click="defeat">{{ $t('debug.enemy.defeat') }}</button>
          </div>
        </section>

        <section class="debug-section">
          <span class="debug-label">{{ $t('debug.common.placeTokens') }}</span>
          <div class="debug-row">
            <button
              v-tooltip="$t('debug.enemy.shiftFive')"
              @click.exact="debug.send(game.id, {tag: 'DamageMessage', contents: {tag: 'DealDamage_', contents: [{tag: 'EnemyTarget', contents: id}, {damageAssignmentSource: {tag: 'InvestigatorSource', contents:investigatorId}, damageAssignmentAmount: 1, damageAssignmentDirect: true, damageAssignmentDelayed: false, damageAssignmentDamageEffect: 'NonAttackDamageEffect'}]}})"
              @click.shift="debug.send(game.id, {tag: 'DamageMessage', contents: {tag: 'DealDamage_', contents: [{tag: 'EnemyTarget', contents: id}, {damageAssignmentSource: {tag: 'InvestigatorSource', contents:investigatorId}, damageAssignmentAmount: 5, damageAssignmentDirect: true, damageAssignmentDelayed: false, damageAssignmentDamageEffect: 'NonAttackDamageEffect'}]}})"
            >{{ $t('debug.enemy.addDamage') }}</button>
            <button v-if="anyTokens" @click="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'ClearTokens_', contents: { tag: 'EnemyTarget', contents: id}}})">{{ $t('debug.common.removeAllTokens') }}</button>
          </div>
          <div class="debug-row">
            <select v-model="placeTokenType">
              <option v-for="token in tokenTypes" :key="token" :value="token">{{ token }}</option>
            </select>
            <button
              v-tooltip="$t('debug.enemy.shiftFive')"
              @click.exact="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{ tag: 'GameSource' }, { tag: 'EnemyTarget', contents: id}, placeTokenType, 1]}})"
              @click.shift="debug.send(game.id, {tag: 'TokenMessage', contents: {tag: 'PlaceTokens_', contents: [{ tag: 'GameSource' }, { tag: 'EnemyTarget', contents: id}, placeTokenType, 5]}})"
            >{{ $t('debug.common.place') }}</button>
          </div>
        </section>

        <section v-if="sealedTokens.length > 0" class="debug-section">
          <span class="debug-label">{{ $t('debug.enemy.sealedTokens') }}</span>
          <div class="debug-row">
            <button
              v-for="token in sealedTokens"
              :key="token.id"
              type="button"
              class="debug-sealed-token"
              v-tooltip="$t('debug.enemy.release')"
              @click="unseal(token)"
            >
              <img :src="chaosTokenImage(token.face)" />
              <span class="debug-sealed-token__x" aria-hidden="true">×</span>
            </button>
          </div>
        </section>

        <section class="debug-section">
          <span class="debug-label">{{ $t('debug.common.modifiers') }}</span>
          <div class="debug-row">
            <button @click="createModifier({tag: 'EnemyTarget', contents: id}, {tag: 'DamageDealt', contents: 1})">{{ $t('debug.enemy.increaseDamageDealt') }}</button>
            <button @click="createModifier({tag: 'EnemyTarget', contents: id}, {tag: 'AddKeyword', contents: {tag: 'Hunter'}})">{{ $t('debug.enemy.addHunter') }}</button>
            <button @click="createModifier({tag: 'EnemyTarget', contents: id}, {tag: 'HealthModifier', contents: 1})">{{ $t('debug.enemy.increaseHealth') }}</button>
          </div>
          <div v-if="enemy.modifiers.length > 0" class="debug-modifiers">
            <Modifier :modifier="modifier" :game="game" v-for="(modifier, idx) in enemy.modifiers" :key="idx" />
          </div>
        </section>

        <button class="debug-close" @click="emit('close')">{{ $t('debug.common.close') }}</button>
      </div>
    </div>
  </Draggable>
</template>

<style scoped>
.enemy {
  display: flex;
  flex-direction: column;
}

/* Section + button vocabulary borrowed from ScenarioDebug.vue so the two debug
   surfaces look like the same tool. */
.debug-panel {
  display: flex;
  flex-direction: column;
  gap: 12px;
  flex: 1;
  min-width: 260px;
  max-width: 340px;
  color: var(--text);
  font-size: 0.85rem;
  font-weight: normal;
}

.debug-section {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px 12px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
}

.debug-label {
  font-family: teutonic, sans-serif;
  font-size: 0.95rem;
  letter-spacing: 0.04em;
  color: rgba(255, 255, 255, 0.72);
}

.debug-row {
  display: flex;
  flex-flow: row wrap;
  align-items: center;
  gap: 6px;
}

.debug-panel button {
  min-width: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  font-size: 0.8rem;
  padding: 7px 10px;
  cursor: pointer;
  transition: background-color 0.12s ease, border-color 0.12s ease;
}

.debug-panel button:hover {
  background: var(--button);
  border-color: var(--select);
}

.debug-panel button.debug-button--danger:hover {
  background: #7a2020;
  border-color: #c05050;
}

.debug-panel select {
  flex: 1;
  min-width: 0;
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 4px;
  background: var(--background-dark);
  color: var(--text);
  font-size: 0.8rem;
  padding: 6px 8px;
}

/* A sealed token doubles as its own release button: the × only shows on hover so
   the row still reads as the enemy's sealed pool at a glance. */
.debug-panel button.debug-sealed-token {
  position: relative;
  padding: 3px;
  display: grid;
  place-items: center;
  border-radius: 999px;
}

.debug-sealed-token img {
  width: 30px;
  height: auto;
  display: block;
}

.debug-sealed-token__x {
  position: absolute;
  inset: 0;
  display: grid;
  place-items: center;
  border-radius: 999px;
  background: rgba(0, 0, 0, 0.6);
  color: #fff;
  font-size: 1.1rem;
  font-weight: 700;
  line-height: 1;
  opacity: 0;
  transition: opacity 0.12s ease;
}

.debug-sealed-token:hover .debug-sealed-token__x {
  opacity: 1;
}

.debug-modifiers {
  display: flex;
  flex-flow: row wrap;
  gap: 4px;
}

.debug-close {
  align-self: flex-end;
}

.enemy--outer {
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: flex-start;
  gap: 10px;
}

.pool {
  position: absolute;
  top: 50%;
  align-items: center;
  width: 100%;
  display: flex;
  flex-wrap: wrap;
  pointer-events: none;
}

.card-frame {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
}

.card {
  width: 300px;
}

.card-no-overlay {
  width: calc(var(--card-width) * 5); 
  max-width: calc(var(--card-width) * 5);
  border-radius: 15px;
  transform: rotate(0deg);
  transition: transform 0.2s linear;
}
</style>
