import { JsonDecoder } from 'ts.data.json';
import { Source, sourceDecoder } from '@/arkham/types/Source';
import { Cost, costDecoder } from '@/arkham/types/Cost';
import { Action, actionDecoder } from '@/arkham/types/Action';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';

export type FastAbility = {
  tag: "FastAbility"
  cost: Cost
}

export const fastAbilityDecoder = JsonDecoder.object<FastAbility>({
  tag: JsonDecoder.isExactly("FastAbility'").map(() => "FastAbility"),
  cost: costDecoder
}, 'FastAbility')

export type ReactionAbility = {
  tag: "ReactionAbility"
  cost: Cost
  // window :: WindowMatcher
}

export const reactionAbilityDecoder = JsonDecoder.object<ReactionAbility>({
  tag: JsonDecoder.isExactly("ReactionAbility"),
  cost: costDecoder
}, 'ReactionAbility')

export type CustomizationReaction = {
  tag: "CustomizationReaction"
  label: string
  cost: Cost
  // window :: WindowMatcher
}

export const customizationReactionDecoder = JsonDecoder.object<CustomizationReaction>({
  tag: JsonDecoder.isExactly("CustomizationReaction"),
  label: JsonDecoder.string,
  cost: costDecoder
}, 'CustomizationReaction')

export type ActionAbility = {
  tag: "ActionAbility"
  actions: Action[]
  cost: Cost
}

export const actionAbilityDecoder = JsonDecoder.object<ActionAbility>({
  tag: JsonDecoder.isExactly("ActionAbility"),
  actions: JsonDecoder.array(actionDecoder, 'Action[]'),
  cost: costDecoder,
}, 'ActionAbility')

export type ActionAbilityWithSkill = {
  tag: "ActionAbilityWithSkill"
  actions: Action[]
  skillType: SkillType
  cost: Cost
}

export const actionAbilityWithSkillDecoder = JsonDecoder.object<ActionAbilityWithSkill>({
  tag: JsonDecoder.isExactly("ActionAbilityWithSkill"),
  actions: JsonDecoder.array(actionDecoder, 'Actions[]'),
  skillType: skillTypeDecoder,
  cost: costDecoder
}, 'ActionAbility')

export type ActionAbilityWithBefore = {
  tag: "ActionAbilityWithBefore"
  actions: Action[]
  actionBefore: Action
  cost: Cost
}

export const actionAbilityWithBeforeDecoder = JsonDecoder.object<ActionAbilityWithBefore>({
  tag: JsonDecoder.isExactly("ActionAbilityWithBefore"),
  actions: JsonDecoder.array(actionDecoder, 'Action[]'),
  actionBefore: actionDecoder,
  cost: costDecoder
}, 'ActionAbility')

export type SilentForcedAbility = {
  tag: "SilentForcedAbility"
}

export const silentForcedAbilityDecoder = JsonDecoder.object<SilentForcedAbility>({
  tag: JsonDecoder.isExactly("SilentForcedAbility"),
}, 'SilentForcedAbility')

export type ForcedAbility = {
  tag: "ForcedAbility"
}

export const forcedAbilityDecoder = JsonDecoder.object<ForcedAbility>({
  tag: JsonDecoder.isExactly("ForcedAbility"),
}, 'ForcedAbility')

export type ForcedAbilityWithCost = {
  tag: "ForcedAbilityWithCost"
  cost: Cost
}

export const forcedAbilityWithCostDecoder = JsonDecoder.object<ForcedAbilityWithCost>({
  tag: JsonDecoder.isExactly("ForcedAbilityWithCost"),
  cost: costDecoder
}, 'ForcedAbilityWithCost')

export type AbilityEffect = {
  tag: "AbilityEffect"
  cost: Cost
}

export const abilityEffectDecoder = JsonDecoder.object<AbilityEffect>({
  tag: JsonDecoder.isExactly("AbilityEffect"),
  cost: costDecoder
}, 'AbilityEffect')

export type Objective = {
  tag: "Objective"
  abilityType: AbilityType
}

export const objectiveDecoder = JsonDecoder.object<Objective>({
  tag: JsonDecoder.isExactly("Objective"),
  abilityType: JsonDecoder.lazy<AbilityType>(() => abilityTypeDecoder)
}, 'Objective')

export type Haunted = {
  tag: "Haunted"
}

export const hauntedDecoder = JsonDecoder.object<Haunted>({
  tag: JsonDecoder.isExactly("Haunted"),
}, 'Haunted')


export type AbilityType = FastAbility | ReactionAbility | CustomizationReaction | ActionAbility | ActionAbilityWithSkill | ActionAbilityWithBefore | SilentForcedAbility | ForcedAbility | ForcedAbilityWithCost | AbilityEffect | Objective | Haunted

export type ForcedWhen = {
  tag: "ForcedWhen"
  abilityType: AbilityType
}

export const forcedWhenDecoder = JsonDecoder.object<ForcedWhen>({
  tag: JsonDecoder.isExactly("ForcedWhen"),
  abilityType: JsonDecoder.lazy(() => abilityTypeDecoder)
}, 'Haunted')


export const abilityTypeDecoder: JsonDecoder.Decoder<AbilityType> = JsonDecoder.oneOf<AbilityType>([
  fastAbilityDecoder,
  reactionAbilityDecoder,
  customizationReactionDecoder,
  actionAbilityDecoder,
  actionAbilityWithSkillDecoder,
  actionAbilityWithBeforeDecoder,
  silentForcedAbilityDecoder,
  forcedAbilityDecoder,
  forcedAbilityWithCostDecoder,
  abilityEffectDecoder,
  objectiveDecoder,
  hauntedDecoder,
  forcedWhenDecoder.map((fw: ForcedWhen) => fw.abilityType)
], 'AbilityType')

export type Ability = {
  type: AbilityType
  source: Source
  tooltip: string | null
  displayAsAction: boolean
}

export const abilityDecoder = JsonDecoder.object(
  {
    type: abilityTypeDecoder,
    source: sourceDecoder,
    tooltip: JsonDecoder.nullable(JsonDecoder.string),
    displayAsAction: JsonDecoder.boolean
  }, 'Ability')
