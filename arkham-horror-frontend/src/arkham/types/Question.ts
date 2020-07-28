import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';

export type Question = ChooseOne | ChoiceResult | ChoiceResults;

export interface ChooseOne {
  tag: 'ChooseOne';
  contents: Question[];
}

export interface ChoiceResult {
  tag: 'ChoiceResult';
  contents: Message;
}

export interface ChoiceResults {
  tag: 'ChoiceResults';
  // contents: Message[];
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
  'ChoiceResult',
);

export const choiceResultsDecoder = JsonDecoder.object<ChoiceResults>(
  {
    tag: JsonDecoder.isExactly('ChoiceResults'),
    // contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChoiceResults',
);

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [
    chooseOneDecoder,
    choiceResultDecoder,
    choiceResultsDecoder,
  ],
  'Question',
);
