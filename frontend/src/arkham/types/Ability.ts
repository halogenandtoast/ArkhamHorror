import { JsonDecoder } from 'ts.data.json';
import { Source, sourceDecoder } from '@/arkham/types/Source';
import { Cost, costDecoder } from '@/arkham/types/Cost';
import { Action, actionDecoder } from '@/arkham/types/Action';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';

export interface FastAbility {
  tag: "FastAbility"
  cost: Cost
}

export const fastAbilityDecoder = JsonDecoder.object<FastAbility>({
  tag: JsonDecoder.isExactly("FastAbility'").map(() => "FastAbility"),
  cost: costDecoder
}, 'FastAbility')

export interface ReactionAbility {
  tag: "ReactionAbility"
  cost: Cost
  // window :: WindowMatcher
}

export const reactionAbilityDecoder = JsonDecoder.object<ReactionAbility>({
  tag: JsonDecoder.isExactly("ReactionAbility"),
  cost: costDecoder
}, 'ReactionAbility')

export interface ActionAbility {
  tag: "ActionAbility"
  action: Action | null
  cost: Cost
}

export const actionAbilityDecoder = JsonDecoder.object<ActionAbility>({
  tag: JsonDecoder.isExactly("ActionAbility"),
  action: JsonDecoder.nullable(actionDecoder),
  cost: costDecoder
}, 'ActionAbility')

export interface ActionAbilityWithSkill {
  tag: "ActionAbilityWithSkill"
  action: Action | null
  skillType: SkillType
  cost: Cost
}

export const actionAbilityWithSkillDecoder = JsonDecoder.object<ActionAbilityWithSkill>({
  tag: JsonDecoder.isExactly("ActionAbilityWithSkill"),
  action: JsonDecoder.nullable(actionDecoder),
  skillType: skillTypeDecoder,
  cost: costDecoder
}, 'ActionAbility')

export interface ActionAbilityWithBefore {
  tag: "ActionAbilityWithBefore"
  action: Action | null
  actionBefore: Action | null
  cost: Cost
}

export const actionAbilityWithBeforeDecoder = JsonDecoder.object<ActionAbilityWithBefore>({
  tag: JsonDecoder.isExactly("ActionAbilityWithBefore"),
  action: JsonDecoder.nullable(actionDecoder),
  actionBefore: JsonDecoder.nullable(actionDecoder),
  cost: costDecoder
}, 'ActionAbility')

export interface SilentForcedAbility {
  tag: "SilentForcedAbility"
}

export const silentForcedAbilityDecoder = JsonDecoder.object<SilentForcedAbility>({
  tag: JsonDecoder.isExactly("SilentForcedAbility"),
}, 'SilentForcedAbility')

export interface ForcedAbility {
  tag: "ForcedAbility"
}

export const forcedAbilityDecoder = JsonDecoder.object<ForcedAbility>({
  tag: JsonDecoder.isExactly("ForcedAbility"),
}, 'ForcedAbility')

export interface ForcedAbilityWithCost {
  tag: "ForcedAbilityWithCost"
  cost: Cost
}

export const forcedAbilityWithCostDecoder = JsonDecoder.object<ForcedAbilityWithCost>({
  tag: JsonDecoder.isExactly("ForcedAbilityWithCost"),
  cost: costDecoder
}, 'ForcedAbilityWithCost')

export interface AbilityEffect {
  tag: "AbilityEffect"
  cost: Cost
}

export const abilityEffectDecoder = JsonDecoder.object<AbilityEffect>({
  tag: JsonDecoder.isExactly("AbilityEffect"),
  cost: costDecoder
}, 'AbilityEffect')

export interface Objective {
  tag: "Objective"
  abilityType: AbilityType
}

export const objectiveDecoder = JsonDecoder.object<Objective>({
  tag: JsonDecoder.isExactly("Objective"),
  abilityType: JsonDecoder.lazy<AbilityType>(() => abilityTypeDecoder)
}, 'Objective')

export interface Haunted {
  tag: "Haunted"
}

export const hauntedDecoder = JsonDecoder.object<Haunted>({
  tag: JsonDecoder.isExactly("Haunted"),
}, 'Haunted')


export type AbilityType = FastAbility | ReactionAbility | ActionAbility | ActionAbilityWithSkill | ActionAbilityWithBefore | SilentForcedAbility | ForcedAbility | ForcedAbilityWithCost | AbilityEffect | Objective | Haunted

export interface ForcedWhen {
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

export interface Ability {
  type: AbilityType
  source: Source
  tooltip: string | null
}

export const abilityDecoder = JsonDecoder.object(
  {
    type: abilityTypeDecoder,
    source: sourceDecoder,
    tooltip: JsonDecoder.nullable(JsonDecoder.string),
  }, 'Ability')
