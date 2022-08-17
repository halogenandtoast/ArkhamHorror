<script lang="ts" setup>
import { computed } from 'vue';
import type { ComputedRef } from 'vue';
import type { Cost } from '@/arkham/types/Cost';
import type { Message, AbilityLabel } from '@/arkham/types/Message';

export interface Props {
 ability: AbilityLabel
}

const props = defineProps<Props>()

const ability: ComputedRef<AbilityLabel> = computed(() => props.ability.ability)

const isAction = (action: string) => {
  if (ability.value.tag === "EvadeLabel") {
    return action === "Evade"
  }
  if (ability.value.tag === "FightEnemy") {
    return action === "Fight"
  }

  const {tag} = ability.value.type
  if (tag !== "ActionAbility" && tag !== "ActionAbilityWithBefore" && tag !== "ActionAbilityWithSkill") {
    return false
  }
  const maction = ability.value.type.action
  return maction === action
}

const isInvestigate = computed(() => isAction("Investigate"))
const isFight = computed(() => isAction("Fight") || ability.value.tag == "FightEnemy")
const isEvade = computed(() => isAction("Evade"))
const isEngage = computed(() => isAction("Engage"))
const display = computed(() => !isAction("Move"))
const isSingleActionAbility = computed(() => {
  return false
  // if (ability.value.tag !== "AbilityLabel") {
  //   return false
  // }
  // const {tag} = ability.value.contents[1].type
  // if (tag !== "ActionAbility" && tag !== "ActionAbilityWithBefore" && tag !== "ActionAbilityWithSkill") {
  //   return false
  // }
  // const costIndex = tag === "ActionAbility" ? 1 : 2
  // const { contents } = ability.value.contents[1].type.contents[costIndex]
  // if (typeof contents?.some == 'function') {
  //   return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 1)
  // } else {
  //   return contents === 1
  // }
})

const tooltip = computed(() => {
  if (ability.value.tag !== "AbilityLabel") {
    return null
  }

  const body = ability.value.contents[1].tooltip
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
  return false
  // if (ability.value.tag !== "AbilityLabel") {
  //   return false
  // }
  // const {tag} = ability.value.contents[1].type
  // if (tag !== "ActionAbility" && tag !== "ActionAbilityWithBefore" && tag !== "ActionAbilityWithSkill") {
  //   return false
  // }
  // const costIndex = tag === "ActionAbility" ? 1 : 2
  // const { contents } = ability.value.contents[1].type.contents[costIndex]
  // if (typeof contents?.some == 'function') {
  //   return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 2)
  // } else {
  //   return contents === 2
  // }
})

const isTripleActionAbility = computed(() => {
  return false
  // if (ability.value.tag !== "AbilityLabel") {
  //   return false
  // }
  // const {tag} = ability.value.contents[1].type
  // if (tag !== "ActionAbility" && tag !== "ActionAbilityWithBefore" && tag !== "ActionAbilityWithSkill") {
  //   return false
  // }
  // const costIndex = tag === "ActionAbility" ? 1 : 2
  // const { contents } = ability.value.contents[1].type.contents[costIndex]
  // if (typeof contents?.some == 'function') {
  //   return contents.some((cost: Cost) => cost.tag == "ActionCost" && cost.contents == 3)
  // } else {
  //   return contents === 3
  // }
})

const isObjective = computed(() => ability.value.tag === "AbilityLabel" && ability.value.type.tag === "Objective")
const isFastActionAbility = computed(() => ability.value.tag === "AbilityLabel" && ability.value.type.tag === "FastAbility")
const isReactionAbility = computed(() => ability.value.tag === "AbilityLabel" && (ability.value.type.tag === "ReactionAbility" || ability.value.type.tag === "LegacyReactionAbility"))
const isForcedAbility = computed(() => ability.value.tag === "AbilityLabel" && ability.value.type.tag === "ForcedAbility")

const isNeutralAbility = computed(() => !(isInvestigate.value || isFight.value || isEvade.value || isEngage.value))

const abilityLabel = computed(() => {
  if (ability.value.tag === "EvadeLabel") {
    return "Evade"
  }

  if (ability.value.tag === "FightEnemy") {
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

  if (ability.value.type.tag === "ActionAbility") {
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
