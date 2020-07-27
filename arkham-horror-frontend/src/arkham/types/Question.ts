import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';

export type Question = ChooseOne | ChoiceResult;

export interface ChooseOne {
  tag: 'ChooseOne';
  contents: Question[];
}

export interface ChoiceResult {
  tag: 'ChoiceResult';
  contents: Message;
}

export const chooseOneDecoder: JsonDecoder.Decoder<ChooseOne> = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.lazy(() => JsonDecoder.array<Question>(questionDecoder, 'Question[]')), // eslint-disable-line
  },
  'ChooseOne',
);

export const choiceResultDecoder = JsonDecoder.object<ChoiceResult>(
  {
    tag: JsonDecoder.isExactly('ChoiceResult'),
    contents: messageDecoder,
  },
  'ArkhamPlayerCard',
);

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [
    chooseOneDecoder,
    choiceResultDecoder,
  ],
  'Question',
);
