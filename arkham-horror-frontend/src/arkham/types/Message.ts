import { JsonDecoder } from 'ts.data.json';

export enum MessageType {
  TAKE_RESOURCES = 'TakeResources',
  DRAW_CARDS = 'DrawCards',
  PLAY_CARD = 'PlayCard',
  INVESTIGATE = 'Investigate',
  END_TURN = 'ChooseEndTurn'
}

export interface Message {
  tag: MessageType;
  contents: any; // eslint-disable-line
}

export const messageTypeDecoder = JsonDecoder.oneOf<MessageType>(
  [
    JsonDecoder.isExactly('TakeResources').then(() => JsonDecoder.constant(MessageType.TAKE_RESOURCES)),
    JsonDecoder.isExactly('DrawCards').then(() => JsonDecoder.constant(MessageType.DRAW_CARDS)),
    JsonDecoder.isExactly('PlayCard').then(() => JsonDecoder.constant(MessageType.PLAY_CARD)),
    JsonDecoder.isExactly('Investigate').then(() => JsonDecoder.constant(MessageType.INVESTIGATE)),
    JsonDecoder.isExactly('ChooseEndTurn').then(() => JsonDecoder.constant(MessageType.END_TURN)),
  ],
  'MessageType',
);

export const messageDecoder = JsonDecoder.object<Message>(
  {
    tag: messageTypeDecoder,
    contents: JsonDecoder.succeed,
  },
  'Message',
);
