import { JsonDecoder } from 'ts.data.json';
import { Ability, abilityDecoder } from '@/arkham/types/Ability';

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

export const tokenTypeDecoder = JsonDecoder.oneOf<TokenType>(
  [ JsonDecoder.isExactly('ResourceToken')
  , JsonDecoder.isExactly('ClueToken')
  , JsonDecoder.isExactly('DamageToken')
  , JsonDecoder.isExactly('HorrorToken')
  , JsonDecoder.isExactly('DoomToken')
  ], 'TokenType');

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

export const investigatorComponentDecoder = JsonDecoder.object<InvestigatorComponent>(
  {
    investigatorId: JsonDecoder.string,
    tokenType: tokenTypeDecoder,
  }, 'InvestigatorComponent');

export const assetComponentDecoder = JsonDecoder.object<AssetComponent>(
  {
    assetId: JsonDecoder.string,
    tokenType: tokenTypeDecoder,
  }, 'InvestigatorComponent');

export const investigatorDeckComponentDecoder = JsonDecoder.object<InvestigatorDeckComponent>(
  {
    investigatorId: JsonDecoder.string,
  }, 'InvestigatorDeckComponent');


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

export interface AbilityLabel {
  tag: 'AbilityLabel';
  investigatorId: string;
  ability: Ability
}

export const abilityLabelDecoder = JsonDecoder.object<AbilityLabel>(
  {
    tag: JsonDecoder.isExactly('AbilityLabel'),
    investigatorId: JsonDecoder.string,
    ability: abilityDecoder,
  }, 'Ability')

export type Message = Label | TargetLabel | ComponentLabel | AbilityLabel | EndTurnButton;

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
    componentLabelDecoder,
    abilityLabelDecoder,
    endTurnButtonDecoder,
  ],
  'Message',
);
