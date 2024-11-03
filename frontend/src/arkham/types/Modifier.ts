import { JsonDecoder } from 'ts.data.json';
import { sourceDecoder, Source } from '@/arkham/types/Source';
import { cardDecoder, Card } from '@/arkham/types/Card';

type CardType = "SkillType"

type CardMatcher
  = { tag: 'CardWithType', contents: CardType }
  | { tag: 'AnyCard' }
  | { tag: 'CardWithOddNumberOfWordsInTitle' }
  | { tag: 'CardWithOddSkillIcons' }

const cardTypeDecoder = JsonDecoder.oneOf<CardType>([
  JsonDecoder.isExactly('SkillType')
], 'CardType')

const cardMatcherDecoder = JsonDecoder.oneOf<CardMatcher>([
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.isExactly('CardWithType'),
      contents: cardTypeDecoder
    },
    'CardWithType'
  ),
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.isExactly('AnyCard')
    },
    'AnyCard'
  ),
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.isExactly('CardWithOddNumberOfWordsInTitle')
    },
    'CardWithOddNumberOfWordsInTitle'
  ),
  JsonDecoder.object<CardMatcher>(
    {
      tag: JsonDecoder.isExactly('CardWithOddSkillIcons')
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
  | OtherModifier
  | SkillModifier
  | UseEncounterDeck
  | CannotCommitCards
  | DoNotDrawConnection
  | Difficulty
  | ScenarioModifier

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

export type AnySkillValue = {
  tag: "AnySkillValue"
  contents: number
}

export type GainVictory = {
  tag: "GainVictory"
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


export type Modifier = {
  type: ModifierType;
  source: Source;
  card?: Card;
}

const modifierTypeDecoder = JsonDecoder.oneOf<ModifierType>([
  JsonDecoder.object<BaseSkillOf>(
    {
      tag: JsonDecoder.isExactly('BaseSkillOf'),
      skillType: JsonDecoder.string,
      value: JsonDecoder.number
    }, 'BaseSkillOf'),
  JsonDecoder.object<BaseSkill>(
    {
      tag: JsonDecoder.isExactly('BaseSkill'),
      contents: JsonDecoder.number
    }, 'BaseSkill'),
  JsonDecoder.object<Difficulty>(
    {
      tag: JsonDecoder.isExactly('Difficulty'),
      contents: JsonDecoder.number
    }, 'Difficulty'),
  JsonDecoder.object<ScenarioModifier>(
    {
      tag: JsonDecoder.isExactly('ScenarioModifier'),
      contents: JsonDecoder.string
    }, 'ScenarioModifier'),
  JsonDecoder.object<DiscoveredClues>(
    {
      tag: JsonDecoder.isExactly('DiscoveredClues'),
      contents: JsonDecoder.number
    }, 'DiscoveredClues'),
  JsonDecoder.object<SkillTestResultValueModifier>(
    {
      tag: JsonDecoder.isExactly('SkillTestResultValueModifier'),
      contents: JsonDecoder.number
    }, 'SkillTestResultValueModifier'),
  JsonDecoder.object<CancelEffects>(
    {
      tag: JsonDecoder.isExactly('CancelEffects')
    }, 'CancelEffects'),
  JsonDecoder.object<CannotPerformSkillTest>(
    {
      tag: JsonDecoder.isExactly('CannotPerformSkillTest')
    }, 'CannotPerformSkillTest'),
  JsonDecoder.object<DamageDealt>(
    {
      tag: JsonDecoder.isExactly('DamageDealt'),
      contents: JsonDecoder.number
    }, 'DamageDealt'),
  JsonDecoder.object<AddSkillValue>(
    {
      tag: JsonDecoder.isExactly('AddSkillValue'),
      contents: JsonDecoder.string
    }, 'AddSkillValue'),
  JsonDecoder.object<UseEncounterDeck>(
    {
      tag: JsonDecoder.isExactly('UseEncounterDeck'),
      contents: JsonDecoder.string
    }, 'UseEncounterDeck'),
  JsonDecoder.object<CannotEnter>(
    {
      tag: JsonDecoder.isExactly('CannotEnter'),
      contents: JsonDecoder.string
    }, 'CannotEnter'),
  JsonDecoder.object<CannotCommitCards>(
    {
      tag: JsonDecoder.isExactly('CannotCommitCards'),
      contents: JsonDecoder.succeed
    }, 'CannotCommitCards'),
  JsonDecoder.object<SkillModifier>(
    {
      tag: JsonDecoder.isExactly('SkillModifier'),
      skillType: JsonDecoder.string,
      value: JsonDecoder.number
    }, 'SkillModifier'),
  JsonDecoder.object<AnySkillValue>(
    {
      tag: JsonDecoder.isExactly('AnySkillValue'),
      contents: JsonDecoder.number
    }, 'AnySkillValue'),
  JsonDecoder.object<GainVictory>(
    {
      tag: JsonDecoder.isExactly('GainVictory'),
      contents: JsonDecoder.number
    }, 'GainVictory'),
  JsonDecoder.object<AddKeyword>(
    {
      tag: JsonDecoder.isExactly('AddKeyword'),
      contents: JsonDecoder.object({ tag: JsonDecoder.string }, "Keyword").map(s => s.tag)
    }, 'AddKeyword'),
  JsonDecoder.object<ActionSkillModifier>(
    {
      tag: JsonDecoder.isExactly('ActionSkillModifier'),
      action: JsonDecoder.string,
      skillType: JsonDecoder.string,
      value: JsonDecoder.number
    }, 'ActionSkillModifier'),
  JsonDecoder.object<DoNotDrawConnection>(
    {
      tag: JsonDecoder.isExactly('DoNotDrawConnection'),
      contents: JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.string], 'DoNotDrawConnection')
    }, 'DoNotDrawConnection'),
  JsonDecoder.object<OtherModifier>({
    tag: JsonDecoder.constant('OtherModifier'),
    contents: JsonDecoder.string
  }, 'OtherModifier', { contents: 'tag'}),
], 'ModifierType');

export const modifierDecoder = JsonDecoder.object<Modifier>({
  type: modifierTypeDecoder,
  source: sourceDecoder,
  card: JsonDecoder.optional(cardDecoder)
}, 'Modifier')
