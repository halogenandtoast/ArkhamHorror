import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { Source, sourceDecoder } from '@/arkham/types/Source';

export type Question = ChooseOne | ChooseUpToN | ChooseSome | ChooseN | ChooseOneAtATime | ChooseOneFromSource | ChooseUpgradeDeck;

export interface ChooseOne {
  tag: 'ChooseOne';
  contents: Message[];
}

export interface ChooseN {
  tag: 'ChooseN';
  contents: Message[];
}

export interface ChooseSome {
  tag: 'ChooseSome';
  contents: Message[];
}

export interface ChooseUpToN {
  tag: 'ChooseUpToN';
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

export interface ChooseUpgradeDeck {
  tag: 'ChooseUpgradeDeck';
}

export const chooseUpgradeDeckDecoder = JsonDecoder.object<ChooseUpgradeDeck>(
  {
    tag: JsonDecoder.isExactly('ChooseUpgradeDeck'),
  },
  'ChooseUpgradeDeck',
);

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOne',
);

export const chooseSomeDecoder = JsonDecoder.object<ChooseSome>(
  {
    tag: JsonDecoder.isExactly('ChooseSome'),
    contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseSome',
);

export const chooseNDecoder = JsonDecoder.object<ChooseN>(
  {
    tag: JsonDecoder.isExactly('ChooseN'),
    contents: JsonDecoder.succeed.map((arr) => arr[1]),
  },
  'ChooseN',
);

export const chooseUpToNDecoder = JsonDecoder.object<ChooseUpToN>(
  {
    tag: JsonDecoder.isExactly('ChooseUpToN'),
    contents: JsonDecoder.succeed.map((arr) => arr[1]),
  },
  'ChooseUpToN',
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
    chooseSomeDecoder,
    chooseUpToNDecoder,
    chooseOneAtATimeDecoder,
    chooseOneFromSourceDecoder,
    chooseUpgradeDeckDecoder,
  ],
  'Question',
);
