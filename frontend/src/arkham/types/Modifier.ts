import { JsonDecoder } from 'ts.data.json';

export type ModifierType = BaseSkillOf | ActionSkillModifier | SkillModifier | UseEncounterDeck | OtherModifier

export interface BaseSkillOf {
  tag: "BaseSkillOf"
  skillType: string
  value: number
}

export interface ActionSkillModifier {
  tag: "ActionSkillModifier"
  action: string
  skillType: string
  value: number
}

export interface SkillModifier {
  tag: "SkillModifier"
  skillType: string
  value: number
}

export interface UseEncounterDeck {
  tag: "UseEncounterDeck"
  contents: string
}

export interface CannotEnter {
  tag: "CannotEnter"
  contents: string
}

export interface OtherModifier {
  tag: string
}


export interface Modifier {
  type: ModifierType;
}

const modifierTypeDecoder = JsonDecoder.oneOf<ModifierType>([
  JsonDecoder.object<BaseSkillOf>(
    {
      tag: JsonDecoder.isExactly('BaseSkillOf'),
      skillType: JsonDecoder.string,
      value: JsonDecoder.number
    }, 'BaseSkillOf'),
  JsonDecoder.object<UseEncounterDeck>(
    {
      tag: JsonDecoder.isExactly('UseEncounterDeck'),
      contents: JsonDecoder.string
    }, 'UseEncounterDeck'),
  JsonDecoder.object<CannotEnter>(
    {
      tag: JsonDecoder.isExactly('CannotEnter'),
      contents: JsonDecoder.string
    }, 'UseEncounterDeck'),
  JsonDecoder.object<SkillModifier>(
    {
      tag: JsonDecoder.isExactly('SkillModifier'),
      skillType: JsonDecoder.string,
      value: JsonDecoder.number
    }, 'SkillModifier'),
  JsonDecoder.object<ActionSkillModifier>(
    {
      tag: JsonDecoder.isExactly('ActionSkillModifier'),
      action: JsonDecoder.string,
      skillType: JsonDecoder.string,
      value: JsonDecoder.number
    }, 'ActionSkillModifier'),
  JsonDecoder.object<OtherModifier>({
  tag: JsonDecoder.string
}, 'ModifierType')], 'ModifierType');

export const modifierDecoder = JsonDecoder.object<Modifier>({
  type: modifierTypeDecoder
}, 'Modifier')
