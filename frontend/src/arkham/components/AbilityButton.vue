<script lang="ts" setup>
import { computed } from 'vue';
import type { ComputedRef } from 'vue';
import type { Cost } from '@/arkham/types/Cost';
import type { AbilityLabel, FightLabel, EvadeLabel } from '@/arkham/types/Message';
import { MessageType } from '@/arkham/types/Message';

export interface Props {
 ability: AbilityLabel | FightLabel | EvadeLabel
}

const props = defineProps<Props>()

const ability: ComputedRef<AbilityLabel> = computed(() => props.ability.ability)

const isAction = (action: string) => {
  if (props.ability.tag === MessageType.EVADE_LABEL) {
    return action === "Evade"
  }
  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return action === "Fight"
  }

  const {tag} = ability.value.type
  if (tag !== "ActionAbility" && tag !== "ActionAbilityWithBefore" && tag !== "ActionAbilityWithSkill") {
    return false
  }
  const maction = ability.value.type.action
  return maction === action
}

function totalActionCost(cost: Cost) {
  if (cost.tag === "Costs") {
    return cost.contents.reduce((acc, v) => {
        if (v.tag === "ActionCost") {
          return acc + v.contents
        }
        return acc
      }, 0)
  } else if (cost.tag === "ActionCost") {
    return cost.contents
  }

  return 0
}

const isInvestigate = computed(() => isAction("Investigate"))
const isFight = computed(() => isAction("Fight"))
const isEvade = computed(() => isAction("Evade"))
const isEngage = computed(() => isAction("Engage"))
const display = computed(() => !isAction("Move"))
const isSingleActionAbility = computed(() => {
  if (!ability.value) {
    return false
  }

  if (ability.value.type.tag !== "ActionAbility") {
    return false
  }

  const cost = ability.value.type.cost
  return totalActionCost(cost) === 1
})

const tooltip = computed(() => {
  const body = ability.value && ability.value.tooltip
  if (body) {
    const content = body.
      replace('{action}', '<span class="action-icon"></span>').
      replace('{fast}', '<span class="fast-icon"></span>').
      replace('{willpower}', '<span class="willpower-icon"></span>').
      replace('{intellect}', '<span class="intellect-icon"></span>').
      replace('{combat}', '<span class="combat-icon"></span>').
      replace('{agility}', '<span class="agility-icon"></span>').
      replace(/_([^_]*)_/g, '<b>$1</b>')
    return { content, html: true }
  }

  return null
})

const isDoubleActionAbility = computed(() => {
  if (!ability.value) {
    return false
  }

  if (ability.value.type.tag !== "ActionAbility") {
    return false
  }

  const cost = ability.value.type.cost
  return totalActionCost(cost) === 2
})

const isTripleActionAbility = computed(() => {
  if (!ability.value) {
    return false
  }

  if (ability.value.type.tag !== "ActionAbility") {
    return false
  }

  const cost = ability.value.type.cost
  return totalActionCost(cost) === 3
})

const isObjective = computed(() => ability.value && ability.value.type.tag === "Objective")
const isFastActionAbility = computed(() => ability.value && ability.value.type.tag === "FastAbility")
const isReactionAbility = computed(() => ability.value && ability.value.type.tag === "ReactionAbility")
const isForcedAbility = computed(() => ability.value && ability.value.type.tag === "ForcedAbility")

const isNeutralAbility = computed(() => !(isInvestigate.value || isFight.value || isEvade.value || isEngage.value))

const abilityLabel = computed(() => {
  if (props.ability.tag === MessageType.EVADE_LABEL) {
    return "Evade"
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return "Fight"
  }

  if (isForcedAbility.value === true) {
    return "Forced"
  }

  if (isObjective.value === true) {
    return "Objective"
  }

  if (isReactionAbility.value === true) {
    return ""
  }

  if (ability.value.type.tag === "ActionAbility" || ability.value.type.tag === "ActionAbilityWithBefore" || ability.value.type.tag === "ActionAbilityWithSkill") {
    const { action } = ability.value.type
    if (action) {
      return action
    }
  }

  return ""
})

const classObject = computed(() => {
  return {
    'ability-button': isSingleActionAbility.value && isNeutralAbility.value,
    'double-ability-button': isDoubleActionAbility.value,
    'triple-ability-button': isTripleActionAbility.value,
    'fast-ability-button': isFastActionAbility.value,
    'reaction-ability-button': isReactionAbility.value,
    'forced-ability-button': isForcedAbility.value,
    'investigate-button': isInvestigate.value,
    'fight-button': isFight.value,
    'evade-button': isEvade.value,
    'engage-button': isEngage.value,
    'objective-button': isObjective.value,
  }
})
</script>

<template>
    <button
      v-if="display"
      class="button"
      :class="classObject"
      @click="$emit('choose', ability)"
      v-tooltip="tooltip"
      >{{abilityLabel}}</button>
</template>

<style lang="scss" scoped>
.button{
  margin-top: 2px;
  color: #fff;
  cursor: pointer;
  border-radius: 4px;
  background-color: #555;
}

.objective-button {
  background-color: #465550;
}

.investigate-button {
  background-color: #40263A;
  &:before {
    font-family: "arkham";
    content: "\0046";
    margin-right: 5px;
  }
}

.fight-button {
  background-color: #8F5B41;
  &:before {
    font-family: "Arkham";
    content: "\0044";
    margin-right: 5px;
  }
}

.evade-button {
  background-color: #576345;
  &:before {
    font-family: "Arkham";
    content: "\0053";
    margin-right: 5px;
  }
}

.engage-button {
  background-color: #555;
  &:before {
    font-family: "Arkham";
    content: "\0048";
    margin-right: 5px;
  }
}

.ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

.double-ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049\0049";
    margin-right: 5px;
  }
}

.triple-ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049\0049\0049";
    margin-right: 5px;
  }
}

.fast-ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0075";
    margin-right: 5px;
  }
}

.forced-ability-button {
  background-color: #222;
  color: #fff;
}

.reaction-ability-button {
  background-color: #A02ECB;
  &:before {
    font-family: "arkham";
    content: "\0059";
    margin-right: 5px;
  }
}

</style>
