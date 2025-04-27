import * as JsonDecoder from 'ts.data.json';
import { sourceDecoder, Source } from '@/arkham/types/Source';
import { cardDecoder, Card } from '@/arkham/types/Card';
import { v2Optional } from '@/arkham/parser';

type CardType = "SkillType"

type CardMatcher
  = { tag: 'CardWithType', contents: CardType }
  | { tag: 'AnyCard' }
  | { tag: 'CardWithOddNumberOfWordsInTitle' }
  | { tag: 'CardWithOddSkillIcons' }

const cardTypeDecoder = JsonDecoder.oneOf<CardType>([
  JsonDecoder.literal('SkillType')
], 'CardType')

const cardMatcherDecoder = JsonDecoder.oneOf<CardMatcher>([
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.literal('CardWithType'),
      contents: cardTypeDecoder
    },
    'CardWithType'
  ),
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.literal('AnyCard')
    },
    'AnyCard'
  ),
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.literal('CardWithOddNumberOfWordsInTitle')
    },
    'CardWithOddNumberOfWordsInTitle'
  ),
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.literal('CardWithOddSkillIcons')
    },
    'CardWithOddSkillIcons'
  )
], 'CardMatcher')

export function cardMatcherToWords(m: CardMatcher): string {
  switch (m.tag) {
    case 'CardWithType':
      switch (m.contents) {
        case 'SkillType':
          return " Skill cards"
        default:
          return " Unknown card type"
      }
    case 'AnyCard':
      return " cards"
    case 'CardWithOddNumberOfWordsInTitle':
      return " cards with odd number of words in title"
    case 'CardWithOddSkillIcons':
      return " cards with odd skill icons"
    default:
      return "Unknown card matcher"
  }
} 

export function cannotCommitCardsToWords(m: { tag: 'CannotCommitCards', contents: CardMatcher }): string {
  return "Cannot commit" + cardMatcherToWords(m.contents)
}

export type ModifierType
  = ActionSkillModifier
  | AddKeyword
  | AddSkillValue
  | AnySkillValue
  | BaseSkill
  | BaseSkillOf
  | CannotEnter
  | DamageDealt
  | DiscoveredClues
  | SkillTestResultValueModifier
  | CancelEffects
  | CannotPerformSkillTest
  | GainVictory
  | ActionCostSetToModifier
  | OtherModifier
  | UIModifier
  | SkillModifier
  | SetSkillValue
  | UseEncounterDeck
  | CannotCommitCards
  | DoNotDrawConnection
  | Difficulty
  | ScenarioModifier
  | RevealAnotherChaosToken
  | DoubleDifficulty
  | DoubleSuccess

export type BaseSkillOf = {
  tag: "BaseSkillOf"
  skillType: string
  value: number
}

export type Difficulty = {
  tag: "Difficulty"
  contents: number
}

export type ScenarioModifier = {
  tag: "ScenarioModifier"
  contents: string
}

export type RevealAnotherChaosToken = {
  tag: "RevealAnotherChaosToken"
}

export type DoubleDifficulty = {
  tag: "DoubleDifficulty"
}

export type DoubleSuccess = {
  tag: "DoubleSuccess"
}

export type CancelEffects = {
  tag: "CancelEffects"
}

export type CannotPerformSkillTest = {
  tag: "CannotPerformSkillTest"
}

export type BaseSkill = {
  tag: "BaseSkill"
  contents: number
}

export type DoNotDrawConnection = {
  tag: "DoNotDrawConnection"
  contents: [string, string]
}

export type DiscoveredClues = {
  tag: "DiscoveredClues"
  contents: number
}

export type SkillTestResultValueModifier = {
  tag: "SkillTestResultValueModifier"
  contents: number
}

export type DamageDealt = {
  tag: "DamageDealt"
  contents: number
}

export type AddSkillValue = {
  tag: "AddSkillValue"
  contents: string
}

export type ActionSkillModifier = {
  tag: "ActionSkillModifier"
  action: string
  skillType: string
  value: number
}

export type SkillModifier = {
  tag: "SkillModifier"
  skillType: string
  value: number
}

export type SetSkillValue = {
  tag: "SetSkillValue"
  skillType: string
  value: number
}

export type AnySkillValue = {
  tag: "AnySkillValue"
  contents: number
}

export type GainVictory = {
  tag: "GainVictory"
  contents: number
}

export type ActionCostSetToModifier = {
  tag: "ActionCostSetToModifier"
  contents: number
}

export type AddKeyword = {
  tag: "AddKeyword"
  contents: string
}

export type UseEncounterDeck = {
  tag: "UseEncounterDeck"
  contents: string
}

export type CannotCommitCards = {
  tag: "CannotCommitCards"
  contents: any
}

export type CannotEnter = {
  tag: "CannotEnter"
  contents: string
}

export type OtherModifier = {
  tag: "OtherModifier"
  contents: string
}

export type UIModifier = {
  tag: "UIModifier"
  contents: string
}


export type Modifier = {
  type: ModifierType;
  source: Source;
  card?: Card;
}

const modifierTypeDecoder = JsonDecoder.oneOf<ModifierType>([
  JsonDecoder.object<BaseSkillOf>(
    {
      tag: JsonDecoder.literal('BaseSkillOf'),
      skillType: JsonDecoder.string(),
      value: JsonDecoder.number()
    }, 'BaseSkillOf'),
  JsonDecoder.object<BaseSkill>(
    {
      tag: JsonDecoder.literal('BaseSkill'),
      contents: JsonDecoder.number()
    }, 'BaseSkill'),
  JsonDecoder.object<Difficulty>(
    {
      tag: JsonDecoder.literal('Difficulty'),
      contents: JsonDecoder.number()
    }, 'Difficulty'),
  JsonDecoder.object<ScenarioModifier>(
    {
      tag: JsonDecoder.literal('ScenarioModifier'),
      contents: JsonDecoder.string()
    }, 'ScenarioModifier'),
  JsonDecoder.object<DiscoveredClues>(
    {
      tag: JsonDecoder.literal('DiscoveredClues'),
      contents: JsonDecoder.number()
    }, 'DiscoveredClues'),
  JsonDecoder.object<SkillTestResultValueModifier>(
    {
      tag: JsonDecoder.literal('SkillTestResultValueModifier'),
      contents: JsonDecoder.number()
    }, 'SkillTestResultValueModifier'),
  JsonDecoder.object<CancelEffects>(
    {
      tag: JsonDecoder.literal('CancelEffects')
    }, 'CancelEffects'),
  JsonDecoder.object<RevealAnotherChaosToken>(
    {
      tag: JsonDecoder.literal('RevealAnotherChaosToken')
    }, 'RevealAnotherChaosToken'),
  JsonDecoder.object<DoubleSuccess>(
    {
      tag: JsonDecoder.literal('DoubleSuccess')
    }, 'DoubleSuccess'),
  JsonDecoder.object<DoubleDifficulty>(
    {
      tag: JsonDecoder.literal('DoubleDifficulty')
    }, 'DoubleDifficulty'),
  JsonDecoder.object<CannotPerformSkillTest>(
    {
      tag: JsonDecoder.literal('CannotPerformSkillTest')
    }, 'CannotPerformSkillTest'),
  JsonDecoder.object<DamageDealt>(
    {
      tag: JsonDecoder.literal('DamageDealt'),
      contents: JsonDecoder.number()
    }, 'DamageDealt'),
  JsonDecoder.object<AddSkillValue>(
    {
      tag: JsonDecoder.literal('AddSkillValue'),
      contents: JsonDecoder.string()
    }, 'AddSkillValue'),
  JsonDecoder.object<UseEncounterDeck>(
    {
      tag: JsonDecoder.literal('UseEncounterDeck'),
      contents: JsonDecoder.string()
    }, 'UseEncounterDeck'),
  JsonDecoder.object<CannotEnter>(
    {
      tag: JsonDecoder.literal('CannotEnter'),
      contents: JsonDecoder.string()
    }, 'CannotEnter'),
  JsonDecoder.object<CannotCommitCards>(
    {
      tag: JsonDecoder.literal('CannotCommitCards'),
      contents: JsonDecoder.succeed()
    }, 'CannotCommitCards'),
  JsonDecoder.object<SkillModifier>(
    {
      tag: JsonDecoder.literal('SkillModifier'),
      skillType: JsonDecoder.string(),
      value: JsonDecoder.number()
    }, 'SkillModifier'),
  JsonDecoder.object<SetSkillValue>(
    {
      tag: JsonDecoder.literal('SetSkillValue'),
      skillType: JsonDecoder.string(),
      value: JsonDecoder.number()
    }, 'SkillModifier'),
  JsonDecoder.object<AnySkillValue>(
    {
      tag: JsonDecoder.literal('AnySkillValue'),
      contents: JsonDecoder.number()
    }, 'AnySkillValue'),
  JsonDecoder.object<GainVictory>(
    {
      tag: JsonDecoder.literal('GainVictory'),
      contents: JsonDecoder.number()
    }, 'GainVictory'),
  JsonDecoder.object<ActionCostSetToModifier>(
    {
      tag: JsonDecoder.literal('ActionCostSetToModifier'),
      contents: JsonDecoder.number()
    }, 'ActionCostSetToModifier'),
  JsonDecoder.object<AddKeyword>(
    {
      tag: JsonDecoder.literal('AddKeyword'),
      contents: JsonDecoder.object({ tag: JsonDecoder.string() }, "Keyword").map(s => s.tag)
    }, 'AddKeyword'),
  JsonDecoder.object<ActionSkillModifier>(
    {
      tag: JsonDecoder.literal('ActionSkillModifier'),
      action: JsonDecoder.string(),
      skillType: JsonDecoder.string(),
      value: JsonDecoder.number()
    }, 'ActionSkillModifier'),
  JsonDecoder.object<DoNotDrawConnection>(
    {
      tag: JsonDecoder.literal('DoNotDrawConnection'),
      contents: JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.string()], 'DoNotDrawConnection')
    }, 'DoNotDrawConnection'),
  JsonDecoder.object<UIModifier>(
    {
      tag: JsonDecoder.literal('UIModifier'),
      contents: JsonDecoder.string(),
    }, 'UIModifier'),
  JsonDecoder.object({
    tag: JsonDecoder.string()
  }, 'OtherModifier').map(({tag}) => ({ tag: 'OtherModifier', contents: tag})),
], 'ModifierType');

export const modifierDecoder = JsonDecoder.object<Modifier>({
  type: modifierTypeDecoder,
  source: sourceDecoder,
  card: v2Optional(cardDecoder)
}, 'Modifier')
