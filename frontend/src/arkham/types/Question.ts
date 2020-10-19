import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { Source, sourceDecoder } from '@/arkham/types/Source';

export type Question = ChooseOne | ChooseN | ChooseOneAtATime | ChooseOneFromSource;

export interface ChooseOne {
  tag: 'ChooseOne';
  contents: Message[];
}

export interface ChooseN {
  tag: 'ChooseN';
  contents: Message[];
}

export interface ChooseOneAtATime {
  tag: 'ChooseOneAtATime';
  contents: Message[];
}

export interface ChooseOneFromSourceContents {
  source: Source;
  choices: Message[];
}

export interface ChooseOneFromSource {
  tag: 'ChooseOneFromSource';
  contents: ChooseOneFromSourceContents;
}

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOne',
);

export const chooseNDecoder = JsonDecoder.object<ChooseN>(
  {
    tag: JsonDecoder.isExactly('ChooseN'),
    contents: JsonDecoder.succeed.map((arr) => arr[1]),
  },
  'ChooseN',
);

export const chooseOneAtATimeDecoder = JsonDecoder.object<ChooseOneAtATime>(
  {
    tag: JsonDecoder.isExactly('ChooseOneAtATime'),
    contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOneAtATime',
);

export const chooseOneFromSourceContentsDecoder = JsonDecoder.object<ChooseOneFromSourceContents>(
  {
    source: sourceDecoder,
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOneFromSourceContents',
);

export const chooseOneFromSourceDecoder = JsonDecoder.object<ChooseOneFromSource>(
  {
    tag: JsonDecoder.isExactly('ChooseOneFromSource'),
    contents: chooseOneFromSourceContentsDecoder,
  },
  'ChooseOneFromSource',
);

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [
    chooseOneDecoder,
    chooseNDecoder,
    chooseOneAtATimeDecoder,
    chooseOneFromSourceDecoder,
  ],
  'Question',
);
