import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';

export type Question = ChooseOne;

export interface ChooseOne {
  tag: 'ChooseOne';
  contents: Message[];
}

export const chooseOneDecoder: JsonDecoder.Decoder<ChooseOne> = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOne',
);

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [
    chooseOneDecoder,
  ],
  'Question',
);
