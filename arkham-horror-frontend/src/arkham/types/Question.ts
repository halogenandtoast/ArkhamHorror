import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { Source, sourceDecoder } from '@/arkham/types/Source';

export type Question = ChooseOne | ChooseOneFromSource;

export interface ChooseOne {
  tag: 'ChooseOne';
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
    chooseOneFromSourceDecoder,
  ],
  'Question',
);
