<script lang="ts" setup>
import PoolItem from '@/arkham/components/PoolItem.vue'
import Key from '@/arkham/components/Key.vue'
import Seal from '@/arkham/components/Seal.vue'
import {computed} from 'vue'
import type { Message } from '@/arkham/types/Message'
import * as Arkham from '@/arkham/types/Investigator'
import type { Game } from '@/arkham/types/Game'
import { useDebug } from '@/arkham/debug'
import { MessageType } from '@/arkham/types/Message'
import { TokenType } from '@/arkham/types/Token'

export interface Props {
  choices: Message[]
  investigator: Arkham.Investigator
  game: Game
  portrait?: boolean
}

const props = withDefaults(defineProps<Props>(), { portrait: false })
const keys = computed(() => props.investigator.keys)
const seals = computed(() => props.investigator.seals)
const id = computed(() => props.investigator.id)
const debug = useDebug()

const doom = computed(() => props.investigator.tokens[TokenType.Doom])
const clues = computed(() => props.investigator.tokens[TokenType.Clue] || 0)
const resources = computed(() => props.investigator.tokens[TokenType.Resource] || 0)
const horror = computed(() => (props.investigator.tokens[TokenType.Horror] || 0) + props.investigator.assignedSanityDamage)
const damage = computed(() => (props.investigator.tokens[TokenType.Damage] || 0) + props.investigator.assignedHealthDamage)
const alarmLevel = computed(() => props.investigator.tokens[TokenType.AlarmLevel] || 0)
const leylines = computed(() => props.investigator.tokens[TokenType.Leyline] || 0)

const healthAction = computed(() => props.choices.findIndex(canAdjustHealth))
const sanityAction = computed(() => props.choices.findIndex(canAdjustSanity))

const takeResourceAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent" && c.component.tokenType === "ResourceToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
})

const spendCluesAction = computed(() => {
  return props.choices
    .findIndex((c) => {
      if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent"  && c.component.tokenType === "ClueToken") {
        return c.component.investigatorId === id.value
      }
      return false
    });
})

function canAdjustHealth(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent" && c.component.tokenType === "DamageToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

function canAdjustSanity(c: Message): boolean {
  if (c.tag === MessageType.COMPONENT_LABEL && c.component.tag === "InvestigatorComponent" && c.component.tokenType === "HorrorToken") {
    return c.component.investigatorId === id.value
  }
  return false
}

</script>

<template>
    <div class="resources">
      <div class="keys" v-if="keys.length > 0">
        <Key v-for="key in keys" :key="key" :name="key" />
      </div>
      <div class="seals" v-if="seals.length > 0">
        <Seal v-for="seal in seals" :key="seal.sealKind" :seal="seal" />
      </div>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'LoseResources', contents: [id, {tag: 'GameSource'}, 1]})"
          @click.shift="debug.send(game.id, {tag: 'LoseAllResources', contents: [id, {tag: 'GameSource'}]})"
        >-</button>
      </template>
      <PoolItem
        type="resource"
        :amount="resources"
        :class="{ 'resource--can-take': takeResourceAction !== -1 }"
        @choose="$emit('choose', takeResourceAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'TakeResources', contents: [id, 1, {tag: 'GameSource' }, false]})"
          @click.shift="debug.send(game.id, {tag: 'TakeResources', contents: [id, 5, {tag: 'GameSource' }, false]})"
        >+</button>
      </template>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, -1]})"
          @click.shift="debug.send(game.id, {tag: 'InvestigatorDiscardAllClues', contents: [{tag: 'GameSource' }, id]})"
        >-</button>
      </template>
      <PoolItem
        type="clue"
        :amount="clues"
        :class="{ 'resource--can-spend': spendCluesAction !== -1 }"
        @choose="$emit('choose', spendCluesAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, 1]})"
          @click.shift="debug.send(game.id, {tag: 'GainClues', contents: [id, {tag: 'GameSource' }, 5]})"
        >+</button>
      </template>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, 1]})"
          @click.shift="debug.send(game.id, {tag: 'HealDamage', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, investigator.tokens['Damage'] ?? 0]})"
        >-</button>
      </template>
      <PoolItem
        type="health"
        :amount="damage"
        :class="{ 'health--can-interact': healthAction !== -1 }"
        @choose="$emit('choose', healthAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 1, 0]})"
          @click.shift="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 5, 0]})"
        >+</button>
      </template>
      <template v-if="debug.active">
        <button
          @click.exact="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, 1]})"
          @click.shift="debug.send(game.id, {tag: 'HealHorror', contents: [{tag: 'InvestigatorTarget', contents: id}, {tag: 'TestSource', contents: []}, investigator.tokens['Horror'] ?? 0]})"
        >-</button>
      </template>
      <PoolItem
        type="sanity"
        :amount="horror"
        :class="{ 'sanity--can-interact': sanityAction !== -1 }"
        @choose="$emit('choose', sanityAction)"
      />
      <template v-if="debug.active">
        <button
          class="plus-button"
          @click.exact="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 1]})"
          @click.shift="debug.send(game.id, {tag: 'InvestigatorDirectDamage', contents: [id, {tag: 'TestSource', contents: []}, 0, 5]})"
        >+</button>
      </template>

      <PoolItem v-if="doom > 0" type="doom" :amount="doom" />

      <PoolItem
        v-if="alarmLevel > 0"
        type="doom"
        :amount="alarmLevel"
        tooltip="Alarm Level"
      />

      <PoolItem
        v-if="leylines > 0"
        type="resource"
        :amount="leylines"
        tooltip="Leyline"
      />
    </div>    
</template>

<style scoped lang="scss">

.resources {
  display: flex;
  justify-content: space-between;
  button {
    height: min-content;
    align-self: center;
  }
  @media (max-width: 800px) and (orientation: portrait)  {
    gap: 8px;
    margin-top: auto;
    width: fit-content;
    :deep(.poolItem) {
      width: calc(var(--pool-token-width)*1.2);
    }
  }
}

.plus-button {
  margin-right: 10px;
}

</style>