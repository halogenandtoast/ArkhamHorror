import { JsonDecoder } from 'ts.data.json';

// data UI msg
//   = Label Text [msg]
//   | TooltipLabel Text Tooltip [msg]
//   | LabelGroup Text [msg]
//   | CardLabel CardCode [msg]
//   | TargetLabel Target [msg]
//   | SkillLabel SkillType [msg]
//   | EvadeLabel EnemyId [msg]
//   | FightLabel EnemyId [msg]
//   | AbilityLabel InvestigatorId Ability [Window] [msg]
//   | ComponentLabel Component [msg]
//   | EndTurnButton InvestigatorId [msg]
//   | StartSkillTestButton InvestigatorId
//   | SkillTestApplyResultsButton
//   | TokenGroupChoice Source InvestigatorId ChaosBagStep
//   | Done Text

export enum MessageType {
  LABEL = 'Label',
  TARGET_LABEL = 'TargetLabel',
  COMPONENT_LABEL = 'ComponentLabel',
}

export interface Label {
  tag: 'Label';
  label: string;
}

export interface Target {
  tag: string;
  contents?: string;
}

export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.optional(JsonDecoder.string),
  },
  'Target',
);

export interface TargetLabel {
  tag: 'TargetLabel';
  target: Target;
}

export interface EndTurnButton {
  tag: 'EndTurnButton';
}

export type Component = InvestigatorComponent | InvestigatorDeckComponent | AssetComponent;

export type TokenType = 'ResourceToken' | 'ClueToken' | 'DamageToken' | 'HorrorToken' | 'DoomToken';

export interface InvestigatorComponent {
  investigatorId: string;
  tokenType: TokenType;
}

export interface AssetComponent {
  assetId: string;
  tokenType: TokenType;
}

export interface InvestigatorDeckComponent {
  investigatorId: string;
}

export const componentDecoder = JsonDecoder.oneOf<Component>(
  [
    investigatorComponentDecoder,
    investigatorDeckComponentDecoder,
    assetComponentDecoder,
  ], 'Component');

export interface ComponentLabel {
  tag: 'ComponentLabel';
  component: Component;
}

export type Message = Label | TargetLabel | EndTurnButton;

export const labelDecoder = JsonDecoder.object<Label>(
  {
    tag: JsonDecoder.isExactly('Label'),
    label: JsonDecoder.string
  }, 'Label')

export const targetLabelDecoder = JsonDecoder.object<TargetLabel>(
  {
    tag: JsonDecoder.isExactly('TargetLabel'),
    target: targetDecoder
  }, 'TargetLabel')

export const componentLabelDecoder = JsonDecoder.object<ComponentLabel>(
  {
    tag: JsonDecoder.isExactly('ComponentLabel'),
    component: componentDecoder
  }, 'ComponentLabel')

export const endTurnButtonDecoder = JsonDecoder.object<EndTurnButton>(
  {
    tag: JsonDecoder.isExactly('EndTurnButton'),
  }, 'EndTurnButton')


export const messageDecoder = JsonDecoder.oneOf<Message>(
  [
    labelDecoder,
    targetLabelDecoder,
    endTurnButtonDecoder,
    // JsonDecoder.isExactly('TooltipLabel').chain(() => JsonDecoder.constant(MessageType.TOOLTIP_LABEL)),
    // JsonDecoder.isExactly('LabelGroup').chain(() => JsonDecoder.constant(MessageType.LABEL_GROUP)),
    // JsonDecoder.isExactly('CardLabel').chain(() => JsonDecoder.constant(MessageType.CARD_LABEL)),
    // JsonDecoder.isExactly('TargetLabel').chain(() => JsonDecoder.constant(MessageType.TARGET_LABEL)),
    // JsonDecoder.isExactly('SkillLabel').chain(() => JsonDecoder.constant(MessageType.SKILL_LABEL)),
    // JsonDecoder.isExactly('EvadeLabel').chain(() => JsonDecoder.constant(MessageType.EVADE_LABEL)),
    // JsonDecoder.isExactly('FightLabel').chain(() => JsonDecoder.constant(MessageType.FIGHT_LABEL)),
    // JsonDecoder.isExactly('AbilityLabel').chain(() => JsonDecoder.constant(MessageType.ABILITY_LABEL)),
    // JsonDecoder.isExactly('ComponentLabel').chain(() => JsonDecoder.constant(MessageType.COMPONENT_LABEL)),
    // JsonDecoder.isExactly('EndTurnButton').chain(() => JsonDecoder.constant(MessageType.END_TURN_BUTTON)),
    // JsonDecoder.isExactly('StartSkillTestButton').chain(() => JsonDecoder.constant(MessageType.START_SKILL_TEST_BUTTON)),
    // JsonDecoder.isExactly('TokenGroupChoice').chain(() => JsonDecoder.constant(MessageType.TOKEN_GROUP_CHOICE)),
    // JsonDecoder.isExactly('Done').chain(() => JsonDecoder.constant(MessageType.DONE)),
  ],
  'Message',
);
