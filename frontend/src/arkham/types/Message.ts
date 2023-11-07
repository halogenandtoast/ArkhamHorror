import { JsonDecoder } from 'ts.data.json';
import { Ability, abilityDecoder } from '@/arkham/types/Ability';
import { chaosBagStepDecoder, ChaosBagStep } from '@/arkham/types/ChaosBag';
import { SkillType, skillTypeDecoder } from '@/arkham/types/SkillType';
import { Target, targetDecoder } from '@/arkham/types/Target';
import { tarotCardDecoder, TarotCard } from '@/arkham/types/TarotCard';

export enum MessageType {
  LABEL = 'Label',
  TARGET_LABEL = 'TargetLabel',
  TOOLTIP_LABEL = 'TooltipLabel',
  SKILL_LABEL = 'SkillLabel',
  CARD_LABEL = 'CardLabel',
  PORTRAIT_LABEL = 'PortraitLabel',
  COMPONENT_LABEL = 'ComponentLabel',
  ABILITY_LABEL = 'AbilityLabel',
  END_TURN_BUTTON = 'EndTurnButton',
  START_SKILL_TEST_BUTTON = 'StartSkillTestButton',
  SKILL_TEST_APPLY_RESULTS_BUTTON = 'SkillTestApplyResultsButton',
  FIGHT_LABEL = 'FightLabel',
  EVADE_LABEL = 'EvadeLabel',
  ENGAGE_LABEL = 'EngageLabel',
  GRID_LABEL = 'GridLabel',
  TAROT_LABEL = 'TarotLabel',
  DONE = 'Done',
  TOKEN_GROUP_CHOICE = 'TokenGroupChoice',
  EFFECT_ACTION_BUTTON = 'EffectActionButton'
}

export interface AbilityMessage {
  contents: AbilityLabel | FightLabel | EvadeLabel
  displayAsAction: boolean
  index: number
}

export interface Done {
  tag: MessageType.DONE
  label: string
}

export interface Label {
  tag: MessageType.LABEL
  label: string
}

export interface TooltipLabel {
  tag: MessageType.TOOLTIP_LABEL
  label: string
  tooltip: string
}

export interface SkillLabel {
  tag: MessageType.SKILL_LABEL
  skillType: SkillType
}

export interface TargetLabel {
  tag: MessageType.TARGET_LABEL
  target: Target
}

export interface EndTurnButton {
  tag: MessageType.END_TURN_BUTTON
  investigatorId: string
}

export type Component = InvestigatorComponent | InvestigatorDeckComponent | AssetComponent;

export type TokenType = 'ResourceToken' | 'ClueToken' | 'DamageToken' | 'HorrorToken' | 'DoomToken';

export const tokenTypeDecoder = JsonDecoder.oneOf<TokenType>(
  [ JsonDecoder.isExactly('ResourceToken')
  , JsonDecoder.isExactly('ClueToken')
  , JsonDecoder.isExactly('DamageToken')
  , JsonDecoder.isExactly('HorrorToken')
  , JsonDecoder.isExactly('DoomToken')
  ], 'TokenType');

export interface InvestigatorComponent {
  tag: "InvestigatorComponent";
  investigatorId: string;
  tokenType: TokenType;
}

export interface AssetComponent {
  tag: "AssetComponent";
  assetId: string;
  tokenType: TokenType;
}

export interface InvestigatorDeckComponent {
  tag: "InvestigatorDeckComponent";
  investigatorId: string;
}

export const investigatorComponentDecoder = JsonDecoder.object<InvestigatorComponent>(
  {
    tag: JsonDecoder.isExactly("InvestigatorComponent"),
    investigatorId: JsonDecoder.string,
    tokenType: tokenTypeDecoder,
  }, 'InvestigatorComponent');

export const assetComponentDecoder = JsonDecoder.object<AssetComponent>(
  {
    tag: JsonDecoder.isExactly("AssetComponent"),
    assetId: JsonDecoder.string,
    tokenType: tokenTypeDecoder,
  }, 'InvestigatorComponent');

export const investigatorDeckComponentDecoder = JsonDecoder.object<InvestigatorDeckComponent>(
  {
    tag: JsonDecoder.isExactly("InvestigatorDeckComponent"),
    investigatorId: JsonDecoder.string,
  }, 'InvestigatorDeckComponent');


export const componentDecoder = JsonDecoder.oneOf<Component>(
  [
    investigatorComponentDecoder,
    investigatorDeckComponentDecoder,
    assetComponentDecoder,
  ], 'Component');

export interface ComponentLabel {
  tag: MessageType.COMPONENT_LABEL
  component: Component
}

export interface FightLabel {
  tag: MessageType.FIGHT_LABEL
  enemyId: string
}

export const fightLabelDecoder = JsonDecoder.object<FightLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.FIGHT_LABEL),
    enemyId: JsonDecoder.string,
  }, 'FightLabel')

export interface EvadeLabel {
  tag: MessageType.EVADE_LABEL
  enemyId: string
}

export const evadeLabelDecoder = JsonDecoder.object<EvadeLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.EVADE_LABEL),
    enemyId: JsonDecoder.string,
  }, 'EvadeLabel')

export interface EngageLabel {
  tag: MessageType.ENGAGE_LABEL
  enemyId: string
}

export const engageLabelDecoder = JsonDecoder.object<EngageLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.ENGAGE_LABEL),
    enemyId: JsonDecoder.string,
  }, 'EngageLabel')

export interface GridLabel {
  tag: MessageType.GRID_LABEL
  gridLabel: string
}

export const gridLabelDecoder = JsonDecoder.object<GridLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.GRID_LABEL),
    gridLabel: JsonDecoder.string,
  }, 'GridLabel')

export interface TarotLabel {
  tag: MessageType.TAROT_LABEL
  tarotCard: TarotCard
}

export const tarotLabelDecoder = JsonDecoder.object<TarotLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.TAROT_LABEL),
    tarotCard: tarotCardDecoder,
  }, 'TarotLabel')

export interface CardLabel {
  tag: MessageType.CARD_LABEL
  cardCode: string
}

export const cardLabelDecoder = JsonDecoder.object<CardLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.CARD_LABEL),
    cardCode: JsonDecoder.string,
  }, 'CardLabel')

export interface PortraitLabel {
  tag: MessageType.PORTRAIT_LABEL
  investigatorId: string
}

export const portraitLabelDecoder = JsonDecoder.object<PortraitLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.PORTRAIT_LABEL),
    investigatorId: JsonDecoder.string,
  }, 'PortraitLabel')

export interface AbilityLabel {
  tag: MessageType.ABILITY_LABEL
  investigatorId: string
  ability: Ability
}

export const abilityLabelDecoder = JsonDecoder.object<AbilityLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.ABILITY_LABEL),
    investigatorId: JsonDecoder.string,
    ability: abilityDecoder,
  }, 'Ability')

export interface StartSkillTestButton {
  tag: MessageType.START_SKILL_TEST_BUTTON
  investigatorId: string
}

export const startSkillTestButtonDecoder = JsonDecoder.object<StartSkillTestButton>(
  {
    tag: JsonDecoder.isExactly(MessageType.START_SKILL_TEST_BUTTON),
    investigatorId: JsonDecoder.string,
  }, 'StartSkillTestButton')

export interface SkillTestApplyResultsButton {
  tag: MessageType.SKILL_TEST_APPLY_RESULTS_BUTTON
}

export const skillTestApplyResultsButtonDecoder = JsonDecoder.object<SkillTestApplyResultsButton>(
  {
    tag: JsonDecoder.isExactly(MessageType.SKILL_TEST_APPLY_RESULTS_BUTTON),
  }, 'SkillTestApplyResultsButton')

export type Message = Label | TooltipLabel | TargetLabel | SkillLabel | CardLabel | PortraitLabel | ComponentLabel | AbilityLabel | EndTurnButton | StartSkillTestButton | SkillTestApplyResultsButton | FightLabel | EvadeLabel | EngageLabel | GridLabel | TarotLabel | Done | TokenGroupChoice | EffectActionButton;

export const doneDecoder = JsonDecoder.object<Done>(
  {
    tag: JsonDecoder.isExactly(MessageType.DONE),
    label: JsonDecoder.string
  }, 'Done')

export const labelDecoder = JsonDecoder.object<Label>(
  {
    tag: JsonDecoder.isExactly(MessageType.LABEL),
    label: JsonDecoder.string
  }, 'Label')

export const tooltipLabelDecoder = JsonDecoder.object<TooltipLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.TOOLTIP_LABEL),
    label: JsonDecoder.string,
    tooltip: JsonDecoder.string,
  }, 'TooltipLabel')

export const skillLabelDecoder = JsonDecoder.object<SkillLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.SKILL_LABEL),
    skillType: skillTypeDecoder
  }, 'SkillLabel')

export const targetLabelDecoder = JsonDecoder.object<TargetLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.TARGET_LABEL),
    target: targetDecoder
  }, 'TargetLabel')

export const componentLabelDecoder = JsonDecoder.object<ComponentLabel>(
  {
    tag: JsonDecoder.isExactly(MessageType.COMPONENT_LABEL),
    component: componentDecoder
  }, 'ComponentLabel')

export const endTurnButtonDecoder = JsonDecoder.object<EndTurnButton>(
  {
    tag: JsonDecoder.isExactly(MessageType.END_TURN_BUTTON),
    investigatorId: JsonDecoder.string,
  }, 'EndTurnButton')

export interface TokenGroupChoice {
  tag: MessageType.TOKEN_GROUP_CHOICE
  investigatorId: string
  step: ChaosBagStep
}

export const tokenGroupChoiceDecoder = JsonDecoder.object<TokenGroupChoice>(
  {
    tag: JsonDecoder.isExactly(MessageType.TOKEN_GROUP_CHOICE),
    investigatorId: JsonDecoder.string,
    step: chaosBagStepDecoder
  }, 'TokenGroupChoice')

export interface EffectActionButton {
  tag: MessageType.EFFECT_ACTION_BUTTON
  effectId: string
  tooltip: string
}

export const effectActionButtonDecoder = JsonDecoder.object<EffectActionButton>(
  {
    tag: JsonDecoder.isExactly(MessageType.EFFECT_ACTION_BUTTON),
    effectId: JsonDecoder.string,
    tooltip: JsonDecoder.string
  }, 'EffectActionButton')


export const messageDecoder = JsonDecoder.oneOf<Message>(
  [
    labelDecoder,
    tooltipLabelDecoder,
    skillLabelDecoder,
    targetLabelDecoder,
    cardLabelDecoder,
    portraitLabelDecoder,
    componentLabelDecoder,
    abilityLabelDecoder,
    endTurnButtonDecoder,
    startSkillTestButtonDecoder,
    skillTestApplyResultsButtonDecoder,
    fightLabelDecoder,
    evadeLabelDecoder,
    engageLabelDecoder,
    gridLabelDecoder,
    tarotLabelDecoder,
    doneDecoder,
    tokenGroupChoiceDecoder,
    effectActionButtonDecoder,
    JsonDecoder.succeed.chain((f) => {
      return JsonDecoder.fail(f)
    })
  ],
  'Message',
);
