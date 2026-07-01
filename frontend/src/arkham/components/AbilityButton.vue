<script lang="ts" setup>
import { computed } from 'vue';
import type { Game } from '@/arkham/types/Game';
import type { Cost } from '@/arkham/types/Cost';
import type { AbilityLabel, FightLabel, FightLabelWithSkill, EvadeLabel, EvadeLabelWithSkill, EngageLabel } from '@/arkham/types/Message';
import { SkillType } from '@/arkham/types/SkillType';
import type { Ability, AbilitySkills, AbilityType } from '@/arkham/types/Ability';
import { sourceKey } from '@/arkham/types/Source';
import type { Action } from '@/arkham/types/Action';
import { actionsToList } from '@/arkham/types/Action';
import { MessageType } from '@/arkham/types/Message';
import { replaceIcons, formatContent } from '@/arkham/helpers';
import { handleI18n } from '@/arkham/i18n';
import { useI18n } from 'vue-i18n';

const { t } = useI18n()
const props = withDefaults(defineProps<{
 game: Game
 ability: AbilityLabel | FightLabel | FightLabelWithSkill | EvadeLabel | EvadeLabelWithSkill | EngageLabel
 tooltipIsButtonText?: boolean
 showMove?: boolean
 hostHasSwarm?: boolean
}>(), { tooltipIsButtonText: false, showMove: true, hostHasSwarm: false })

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
  return props.game.modifiers.filter(([target]) => {
    const contents = target.contents
    if (target.tag !== "AbilityRef" || typeof contents !== 'object' || contents === null || !('ability' in contents)) {
      return false
    }

    if (!ability.value) return false
    return sourceKey(contents.ability.source) === sourceKey(ability.value.source)
      && contents.ability.index === ability.value.index
  })
})

const labelType = computed(() => {
  const ty = ability.value?.type ?? null
  if (ty && ty.tag === "DelayedAbility") return ty.abilityType
  return ty
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
  if (props.ability.tag === MessageType.EVADE_LABEL_WITH_SKILL) {
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
    const actions = actionsToList(ability.value.type.actions)
    return actions.indexOf(action) !== -1
  }

  return false
}

function totalActionCost(cost: Cost): number {
  if (cost.tag === "Costs") {
    const contents = (cost as { contents: Cost[] }).contents
    return contents.reduce<number>((acc, v) => v.tag === "ActionCost" ? acc + Number((v as { contents: number }).contents) : acc, 0)
  } else if (cost.tag === "ActionCost") {
    const setActions = modifiers.value.find((m) => m[1][0]?.type.tag === "ActionCostSetToModifier")
    const modifierType = setActions?.[1][0]?.type
    if (modifierType?.tag === "ActionCostSetToModifier") {
      return modifierType.contents
    }
    return Number((cost as { contents: number }).contents)
  }

  return 0
}

function abilityTypeCost(type: AbilityType): Cost | null {
  switch (type.tag) {
    case 'FastAbility':
    case 'ReactionAbility':
    case 'CustomizationReaction':
    case 'ConstantReaction':
    case 'ActionAbility':
    case 'ForcedAbilityWithCost':
    case 'AbilityEffect':
      return type.cost
    default:
      return null
  }
}

const isInvestigate = computed(() => isAction("Investigate"))
const isFight = computed(() => isAction("Fight"))
const isEvade = computed(() => isAction("Evade"))
const isEngage = computed(() => isAction("Engage"))

const tformat = (t:string) => t.startsWith("$") ? t.slice(1) : t
const maybeFormat = function(body: string) {
  return body.startsWith("$") ? handleI18n(tformat(body), t) : body
}

const abilityLabel = computed(() => {
  // don't use isButtonText
  if (isButtonText.value && tooltip.value) {
    return tooltip.value.content
  }

  if (props.ability.tag === MessageType.ABILITY_LABEL) {
    if (props.ability.ability.displayAs === 'DisplayAsAction') {
      const cost = ability.value ? abilityTypeCost(ability.value.type) : null
      return cost ? replaceIcons("{action}".repeat(totalActionCost(cost))) : ''
    }
    if (props.ability.ability.displayAs === 'DisplayAsCard') {
      return props.ability.ability.tooltip ? formatContent(maybeFormat(props.ability.ability.tooltip)) : ''
    }
  }

  if (props.ability.tag === MessageType.EVADE_LABEL) {
    return t('Evade')
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return t('Fight')
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL_WITH_SKILL) {
    return t('Fight')
  }

  if (props.ability.tag === MessageType.EVADE_LABEL_WITH_SKILL) {
    return t('Evade')
  }

  if (props.ability.tag === MessageType.ENGAGE_LABEL) {
    return t('Engage')
  }

  if (isDelayedAbility.value === true) {
    return t('Delayed')
  }

  if (labelType.value?.tag === "ForcedAbility") {
    return t('Forced')
  }

  if (labelType.value?.tag === "Objective") {
    return t('Objective')
  }

  if (labelType.value?.tag === "CustomizationReaction") {
    return labelType.value.label
  }

  if (labelType.value?.tag === "ConstantReaction") {
    return labelType.value.label
  }

  if (labelType.value?.tag === "ServitorAbility") {
    return labelType.value.action
  }

  if (labelType.value?.tag === "ReactionAbility") {
    return t('Reaction')
  }

  if (labelType.value?.tag === "ActionAbility") {
    const { actions, cost } = labelType.value
    const total = totalActionCost(cost)
    const actionPrefix = total > 0 ? `<span>${replaceIcons("{action}".repeat(total))}</span>` : ""

    if (actions.tag === "OrActions") {
      const labels = actions.contents.map(a => actionsToList(a).map(n => t(n)).join(" "))
      return `${actionPrefix}<span>${t('slashOr', labels)}</span>`
    }

    const asList = actionsToList(actions)
    if (asList.length === 1) {
      return `${actionPrefix}<span>${t(asList[0])}</span>`
    }

    return replaceIcons("{action}".repeat(totalActionCost(cost)))
  }

  if (labelType.value?.tag === "Haunted") {
    return t('Haunted')
  }

  return ""
})
const abilitySkillSection = computed(() => isButtonText.value ? null : abilityString.value)
const display = computed(() => !(isAction("Move") && ability.value?.index === 104) || props.showMove)
const showSwarmHostWarning = computed(() => props.hostHasSwarm && isFight.value)
const swarmHostWarningTooltip = 'This host cannot be defeated while it has swarm cards attached.'

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

const abilitySkills = computed<AbilitySkills | null>(() => {
  if (props.ability.tag === MessageType.FIGHT_LABEL_WITH_SKILL) {
    return { tag: "AbilitySkill", contents: props.ability.skillType }
  }

  if (props.ability.tag === MessageType.FIGHT_LABEL) {
    return { tag: "AbilitySkill", contents: "SkillCombat" }
  }

  if (props.ability.tag === MessageType.EVADE_LABEL_WITH_SKILL) {
    return { tag: "AbilitySkill", contents: props.ability.skillType }
  }

  if (props.ability.tag === MessageType.EVADE_LABEL) {
    return { tag: "AbilitySkill", contents: "SkillAgility" }
  }

  if (!ability.value) {
    return null
  }

  if (ability.value.type.tag === "ActionAbility") {
    return ability.value.type.skillTypes
  }

  return null
})

function abilitySkillsContain(skills: AbilitySkills, skillType: SkillType): boolean {
  switch (skills.tag) {
    case 'AbilitySkill':
      return skills.contents === skillType
    case 'AndAbilitySkills':
    case 'OrAbilitySkills':
      return skills.contents.some((s) => abilitySkillsContain(s, skillType))
  }
}

function isSkill(skillType: SkillType) {
  return abilitySkills.value ? abilitySkillsContain(abilitySkills.value, skillType) : false
}

const abilityString = computed(() => {
  if (!abilitySkills.value) {
    return null
  }

  const toString = (a: AbilitySkills): string | null => {
    switch (a.tag) {
      case "AbilitySkill": {
        return toSkill(a.contents)
      }
      case "OrAbilitySkills": {
        return t(`or${a.contents.length}`, a.contents.map(toString))
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
  >
    <span
      v-if="showSwarmHostWarning"
      class="swarm-host-warning"
      v-tooltip="swarmHostWarningTooltip"
      @click.stop
    >
      <font-awesome-icon icon="triangle-exclamation" aria-hidden="true" />
    </span>
    <span class="button-label" v-html="abilityLabel" />
    <span v-if="abilitySkillSection" class="button-skill-section" v-html="abilitySkillSection" />
  </button>
</template>

<style scoped>
.button{
  border: 0;
  margin-top: 2px;
  color: #fff;
  cursor: pointer;
  border-radius: 4px;
  background-color: var(--button);
  z-index: var(--z-index-1000);
  width: 100%;
  min-width: max-content;
  display: inline-flex;
  align-items: stretch;
  justify-content: center;
  gap: 0;
  padding: 0;
  overflow: hidden;
}

.button-label {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex: 1 1 auto;
  padding: 3px 6px;
}

.button-label:empty {
  display: none;
}

.button::before {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  align-self: stretch;
}

.button.ability-button::before,
.button.zeroed-ability-button::before,
.button.fast-ability-button::before,
.button.reaction-ability-button::before {
  padding: 3px 6px;
  margin-right: 0;
}

.button-skill-section {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 3px;
  align-self: stretch;
  padding: 3px 6px;
  background: rgba(0, 0, 0, 0.14);
  border-left: 1px solid rgba(255, 255, 255, 0.18);
  white-space: nowrap;
}

.swarm-host-warning {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 1.8em;
  min-width: 1.8em;
  align-self: stretch;
  margin: 0;
  padding: 0;
  background: rgba(0, 0, 0, 0.34);
  border-right: 1px solid rgba(255, 255, 255, 0.24);
  border-radius: 4px 0 0 4px;
  color: #ffd166;
}

.swarm-host-warning svg {
  display: block;
  width: 1em;
  height: 1em;
  filter: drop-shadow(0 1px 1px rgba(0, 0, 0, 0.65));
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

  &.skill-combat-to-intellect-to-willpower {
    background: linear-gradient(90deg, var(--combat) 0%, var(--intellect) 50%, var(--willpower) 100%);
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
  background-color: var(--button);
}

.ability-button {
  background-color: var(--button);
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
  }
}

.zeroed-ability-button {
  background-color: var(--button);
  &:before {
    font-family: "arkham";
    content: "\0049";
    margin-right: 5px;
    color: rgba(255, 255, 255, 0.5);
  }
}

.fast-ability-button {
  background-color: var(--button);
  &:before {
    font-family: "arkham";
    content: "\0075";
    margin-right: 5px;
  }
}

.forced-ability-button, button.forced-ability-button {
  background-color: var(--neutral-extra-dark);
  border: 2px solid var(--select);
  color: #fff;
}

.delayed-ability-button {
  background-color: var(--neutral-extra-dark);
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
