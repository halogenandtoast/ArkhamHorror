<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Cost } from '@/arkham/types/Cost';
import type { AbilityLabel, AbilitySkills, FightLabel, FightLabelWithSkill, EvadeLabel, EngageLabel } from '@/arkham/types/Message';
import { SkillType } from '@/arkham/types/SkillType';
import type { Ability } from '@/arkham/types/Ability';
import type { Action } from '@/arkham/types/Action';
import { MessageType } from '@/arkham/types/Message';
import { replaceIcons } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { useI18n } from 'vue-i18n';

const { t } = useI18n()
const props = withDefaults(defineProps<{
 game: Game
 ability: AbilityLabel | FightLabel | FightLabelWithSkill | EvadeLabel | EngageLabel
 tooltipIsButtonText?: boolean
 showMove?: boolean
}>(), { tooltipIsButtonText: false, showMove: true })

const ability = computed<Ability | null>(() => "ability" in props.ability ? props.ability.ability : null)

const attributes = computed(() => {
  if (ability.value && ability.value.target) {
    return { 'data-highlight-id': ability.value.target.contents }
  }

  return {}
})

const tooltip = computed(() => {
  var body = ability.value && ability.value.tooltip
  if (body) {
    body = body.startsWith("$") ? handleI18n(body, t) : body
    const content = replaceIcons(body).replace(/_([^_]*)_/g, '<b>$1</b>')
    return { content, html: true }
  }

  return null
})

const modifiers = computed(() => {
  return props.game.modifiers.filter((m) => {
    if (m[0].tag === "AbilityRef") {
      return m[0].contents.ability.source.contents === ability.value?.source?.contents
        && m[0].contents.ability.index === ability.value?.index
    }
    return false
  })
})

const isObjective = computed(() => ability.value && ability.value.type.tag === "Objective")
const isFastActionAbility = computed(() => ability.value && ability.value.type.tag === "FastAbility")
const isReactionAbility = computed(() => ability.value && ability.value.type.tag === "ReactionAbility")
const isForcedAbility = computed(() => ability.value && ability.value.type.tag === "ForcedAbility")
const isDelayedAbility = computed(() => ability.value && ability.value.type.tag === "DelayedAbility")
const isHaunted = computed(() => ability.value && ability.value.type.tag === "Haunted")

const isNeutralAbility = computed(() => !(isInvestigate.value || isFight.value || isEvade.value || isEngage.value))

const isButtonText = computed(() => {
  return (props.tooltipIsButtonText && tooltip.value) || (tooltip.value && tooltip.value.content == "Use True Magick")
})

const isAction = (action: Action) => {
  if (props.ability.tag === MessageType.ABILITY_LABEL) {
    if (props.ability.ability.displayAs === 'DisplayAsAction') {
      return false
    }
    if (props.ability.ability.displayAs === 'DisplayAsCard') {
      return false
    }
  }

  if (props.ability.tag === MessageType.EVADE_LABEL) {
    return action === "Evade"
  }
  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return action === "Fight"
  }
  if (props.ability.tag === MessageType.FIGHT_LABEL_WITH_SKILL) {
    return action === "Fight"
  }
  if (props.ability.tag === MessageType.ENGAGE_LABEL) {
    return action === "Engage"
  }

  if (ability.value) {
    const {tag} = ability.value.type
    if (tag !== "ActionAbility") {
      return false
    }
    const actions = ability.value.type.actions
    return actions.indexOf(action) !== -1
  }

  return false
}

function totalActionCost(cost: Cost) {
  if (cost.tag === "Costs") {
    return cost.contents.reduce((acc, v) => v.tag === "ActionCost" ? acc + v.contents : acc, 0)
  } else if (cost.tag === "ActionCost") {
    const setActions = modifiers.value.find((m) => m[1][0].type.tag === "ActionCostSetToModifier")
    if (setActions) {
      return setActions[1][0].type.contents
    }
    return cost.contents
  }

  return 0
}

const isInvestigate = computed(() => isAction("Investigate"))
const isFight = computed(() => isAction("Fight"))
const isEvade = computed(() => isAction("Evade"))
const isEngage = computed(() => isAction("Engage"))

const abilityLabel = computed(() => {
  // don't use isButtonText
  if (isButtonText.value && tooltip.value) {
    return tooltip.value.content
  }

  if (props.ability.tag === MessageType.ABILITY_LABEL) {
    if (props.ability.ability.displayAs === 'DisplayAsAction') {
      const { cost } = ability.value.type
      return replaceIcons("{action}".repeat(totalActionCost(cost)))
    }
    if (props.ability.ability.displayAs === 'DisplayAsCard') {
      return replaceIcons(props.ability.ability.tooltip)
    }
  }

  if (props.ability.tag === MessageType.EVADE_LABEL) {
    return `${t('Evade')} (<i class='skill-icon'></i>)`
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return `${t('Fight')} (${abilityString.value ? abilityString.value : '<i class="skill-icon skill-combat"></i>'})`
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL_WITH_SKILL) {
    return `${t('Fight')} (${abilityString.value ? abilityString.value : '<i class="skill-icon skill-fight"></i>'})`
  }

  if (props.ability.tag === MessageType.ENGAGE_LABEL) {
    return t('Engage')
  }

  if (isForcedAbility.value === true) {
    return t('Forced')
  }

  if (isDelayedAbility.value === true) {
    return t('Delayed')
  }

  if (isObjective.value === true) {
    return t('Objective')
  }

  if (ability.value && ability.value.type.tag === "CustomizationReaction") {
    return ability.value.type.label
  }

  if (ability.value && ability.value.type.tag === "ConstantReaction") {
    return ability.value.type.label
  }

  if (ability.value && ability.value.type.tag === "ServitorAbility") {
    return ability.value.type.action
  }

  if (isReactionAbility.value === true) {
    return ""
  }

  if (ability.value && ability.value.type.tag === "ActionAbility") {
    const { actions, cost } = ability.value.type
    const total = totalActionCost(cost)
    const skillIcon = abilityString.value ? ` (${abilityString.value})` : ""
    if (actions.length === 1) {
      return `${total > 0 ? `<span>${replaceIcons("{action}".repeat(total))}</span>` : ""}<span>${t(actions[0])}</span>${skillIcon}`
    }

    return replaceIcons("{action}".repeat(totalActionCost(cost)))
  }

  if (isHaunted.value === true) {
    return t('Haunted')
  }

  return ""
})
const display = computed(() => !(isAction("Move") && ability.value.index === 102) || props.showMove)

const isZeroedActionAbility = computed(() => {
  if (!ability.value) {
    return false
  }

  if (ability.value.type.tag !== "ActionAbility") {
    return false
  }

  const cost = ability.value.type.cost
  return totalActionCost(cost) === 0
})

const abilitySkills = computed(() => {
  if (props.ability.tag === MessageType.FIGHT_LABEL_WITH_SKILL) {
    return { tag: "AbilitySkill", contents: props.ability.skillType }
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return { tag: "AbilitySkill", contents: "SkillCombat" }
  }

  if (!ability.value) {
    return null
  }

  if (ability.value.type.tag === "ActionAbility") {
    return ability.value.type.skillTypes
  }

  return null
})

function isSkill(skillType: string) {
  if (!abilitySkills.value) {
    return false
  }

  if (abilitySkills.value.tag === "AbilitySkill") {
    return abilitySkills.value.contents === skillType
  }

  return abilitySkills.value.contents.indexOf(skillType) !== -1
}

const abilityString = computed(() => {
  if (!abilitySkills.value) {
    return null
  }

  const toString = (a: AbilitySkills) => {
    switch (a.tag) {
      case "AbilitySkill": {
        return toSkill(a.contents)
      }
      case "OrAbilitySkills": {
        return t("or", a.contents.map(toString))
      }
      case "AndAbilitySkills": {
        return t("and", a.contents.map(toString))
      }
      default: return null
    }
  }

  const toSkill = (skillType: SkillType) => {
    switch (skillType) {
      case "SkillAgility":
        return "<i class='skill-icon skill-agility'></i>"
      case "SkillCombat":
        return "<i class='skill-icon skill-combat'></i>"
      case "SkillIntellect":
        return "<i class='skill-icon skill-intellect'></i>"
      case "SkillWillpower":
        return "<i class='skill-icon skill-willpower'></i>"
      default:
        return ""
    }
  }

  return toString(abilitySkills.value)
})

const classObject = computed(() => {
  if (isButtonText.value) {
    return {}
  }
  // want to take the ability skills and create a class like `skill-combat-or-agility` and the like based on the AbilitySkills, OrAbilitySkills should kebab with `or` and AndAbilitySkills should kebab with `and`

  const toClass = (a: AbilitySkills): string => {
    switch (a.tag) {
      case "AbilitySkill": {
        return toSkill(a.contents)
      }
      case "OrAbilitySkills": {
        return a.contents.map(toClass).join("-to-")
      }
      case "AndAbilitySkills": {
        return a.contents.map(toClass).join("-to-")
      }
      default: return ""
    }
  }

  const toSkill = (skillType: SkillType): string => {
    switch (skillType) {
      case "SkillAgility":
        return "agility"
      case "SkillCombat":
        return "combat"
      case "SkillIntellect":
        return "intellect"
      case "SkillWillpower":
        return "willpower"
      default:
        return ""
    }
  }

  const abilitySkillClass = abilitySkills.value ? `skill-${toClass(abilitySkills.value)}` : null

  return {
    'zeroed-ability-button': isZeroedActionAbility.value && isNeutralAbility.value,
    'fast-ability-button': isFastActionAbility.value,
    'reaction-ability-button': isReactionAbility.value,
    'forced-ability-button': isForcedAbility.value,
    'delayed-ability-button': isDelayedAbility.value,
    'investigate-button': isInvestigate.value,
    'investigate-button--agility': isInvestigate.value && isSkill("SkillAgility"),
    'investigate-button--combat': isInvestigate.value && isSkill("SkillCombat"),
    'investigate-button--willpower': isInvestigate.value && isSkill("SkillWillpower"),
    'fight-button': isFight.value,
    'fight-button--agility': isFight.value && isSkill("SkillAgility"),
    'fight-button--intellect': isFight.value && isSkill("SkillIntellect"),
    'fight-button--willpower': isFight.value && isSkill("SkillWillpower"),
    'evade-button': isEvade.value,
    'evade-button--combat': isEvade.value && isSkill("SkillCombat"),
    'evade-button--intellect': isEvade.value && isSkill("SkillIntellect"),
    'evade-button--willpower': isEvade.value && isSkill("SkillWillpower"),
    'engage-button': isEngage.value,
    'objective-button': isObjective.value,
    [abilitySkillClass as string]: abilitySkillClass !== null
  }
})
</script>

<template>
  <button
    v-if="display"
    class="button"
    :class="classObject"
    @click="$emit('choose', ability)"
    v-bind="attributes"
    v-tooltip="!isButtonText && tooltip"
    v-html="abilityLabel"
    ></button>
</template>

<style scoped>
.button{
  border: 0;
  margin-top: 2px;
  color: #fff;
  cursor: pointer;
  border-radius: 4px;
  background-color: #555;
  z-index: 1000;
  width: 100%;
  min-width: max-content;
}

.button:has(.skill-icon) {
  text-align: left;
  display: block;
}

.objective-button {
  background-color: #465550;
}

.button:has(.skill-intellect) {
  background-color: var(--intellect);
}

.button {
  &.skill-intellect-to-agility {
    background: linear-gradient(90deg, var(--intellect) 0%, var(--agility) 100%);
  }
  &.skill-intellect-to-willpower {
    background: linear-gradient(90deg, var(--intellect) 0%, var(--willpower) 100%);
  }
  &.skill-intellect-to-combat {
    background: linear-gradient(90deg, var(--intellect) 0%, var(--combat) 100%);
  }

  &.skill-agility-to-intellect {
    background: linear-gradient(90deg, var(--agility) 0%, var(--intellect) 100%);
  }
  &.skill-agility-to-combat {
    background: linear-gradient(90deg, var(--agility) 0%, var(--combat) 100%);
  }
  &.skill-agility-to-willpower {
    background: linear-gradient(90deg, var(--agility) 0%, var(--willpower) 100%);
  }


  &.skill-willpower-to-intellect {
    background: linear-gradient(90deg, var(--willpower) 0%, var(--intellect) 100%);
  }
  &.skill-willpower-to-agility {
    background: linear-gradient(90deg, var(--willpower) 0%, var(--agility) 100%);
  }
  &.skill-willpower-to-combat {
    background: linear-gradient(90deg, var(--willpower) 0%, var(--combat) 100%);
  }

  &.skill-combat-to-agility {
    background: linear-gradient(90deg, var(--combat) 0%, var(--agility) 100%);
  }
  &.skill-combat-to-willpower {
    background: linear-gradient(90deg, var(--combat) 0%, var(--willpower) 100%);
  }
  &.skill-combat-to-intellect {
    background: linear-gradient(90deg, var(--combat) 0%, var(--intellect) 100%);
  }
}

.button:has(.skill-combat) {
  background-color: var(--combat);
}

.button:has(.skill-agility) {
  background-color: var(--agility);
}

.button:has(.skill-willpower) {
  background-color: var(--willpower);
}

:deep(.skill-intellect) {
  --skill-icon: "\0046";
}

:deep(.skill-combat) {
  --skill-icon: "\0044";
}

:deep(.skill-agility) {
  --skill-icon: "\0053";
}

:deep(.skill-willpower) {
  --skill-icon: "\0041";
}

:deep(.skill-icon:before) {
  font-family: "arkham";
  font-style: normal;
  content: var(--skill-icon);
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

.zeroed-ability-button {
  background-color: #555;
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
    color: rgba(255, 255, 255, 0.5);
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

.forced-ability-button, button.forced-ability-button {
  background-color: #222;
  border: 2px solid var(--select);
  color: #fff;
}

.delayed-ability-button {
  background-color: #222;
  border: 2px solid var(--select);
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
