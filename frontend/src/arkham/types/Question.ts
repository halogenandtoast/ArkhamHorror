import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { Source, sourceDecoder } from '@/arkham/types/Source';

export type Question = ChooseOne | ChooseUpToN | ChooseSome | ChooseN | ChooseOneAtATime | ChooseOneFromSource | ChooseUpgradeDeck | ChoosePaymentAmounts | ChooseDynamicCardAmounts | ChooseAmounts | QuestionLabel;

export interface ChooseOne {
  tag: 'ChooseOne';
  contents: Message[];
}

export interface QuestionLabel {
  tag: 'QuestionLabel';
  contents: [string, Question];
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

export interface ChoosePaymentAmounts {
  tag: 'ChoosePaymentAmounts';
  contents: [string, number | null, [string, [number, number]][]];
}

export interface ChooseAmounts {
  tag: 'ChooseAmounts';
  contents: [string, number, [string, [number, number]][]];
}

export interface ChooseDynamicCardAmounts {
  tag: 'ChooseDynamicCardAmounts';
  contents: [string, string, [number, number]]
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

export const chooseAmountsDecoder = JsonDecoder.object<ChooseAmounts>(
  {
    tag: JsonDecoder.isExactly('ChooseAmounts'),
    contents: JsonDecoder.tuple(
      [ JsonDecoder.string
      , JsonDecoder.number
      , JsonDecoder.array(
          JsonDecoder.tuple(
            [ JsonDecoder.string
            , JsonDecoder.tuple(
              [ JsonDecoder.number
              , JsonDecoder.number
              ]
              , '[number, number]')
            ]
            , '[string, [number, number]]'
          )
          , '[string, [number, number]][]')
      , JsonDecoder.succeed
      ]
      , '[string, number, [string, [number, number]]]').map(([label, maxBounds, choices]) => [label,maxBounds, choices])
  }, 'ChooseAmounts',
);

export const choosePaymentAmountsDecoder = JsonDecoder.object<ChoosePaymentAmounts>(
  {
    tag: JsonDecoder.isExactly('ChoosePaymentAmounts'),
    contents: JsonDecoder.tuple(
      [ JsonDecoder.string
      , JsonDecoder.nullable(JsonDecoder.number)
      , JsonDecoder.array(
          JsonDecoder.tuple(
            [ JsonDecoder.string
            , JsonDecoder.tuple(
              [ JsonDecoder.number
              , JsonDecoder.number
              ]
              , '[number, number]')
            , JsonDecoder.succeed
            ]
            , '[string, [number, number]]'
          ).map<[string, [number, number]]>(([iid, bounds]) => { return [iid, bounds] })
          , '[string, [number, number]][]')
      ]
      , '[string, number?, [string, [number, number]]]')
  }, 'ChoosePaymentAmounts',
);

export const chooseDynamicCardAmountsDecoder = JsonDecoder.object<ChooseDynamicCardAmounts>(
  {
    tag: JsonDecoder.isExactly('ChooseDynamicCardAmounts'),
    contents: JsonDecoder.tuple(
      [ JsonDecoder.string
      , JsonDecoder.string
      , JsonDecoder.tuple([ JsonDecoder.number , JsonDecoder.number ], '[number, number]')
      , JsonDecoder.succeed
      , JsonDecoder.succeed
      ]
      , '[string, string, [number, number], boolean, messages]').map<[string, string, [number, number]]>(([iid, cardId, bounds]) => { return [iid, cardId, bounds] })
  }, 'ChoosePaymentAmounts',
);


export const chooseUpgradeDeckDecoder = JsonDecoder.object<ChooseUpgradeDeck>(
  {
    tag: JsonDecoder.isExactly('ChooseUpgradeDeck'),
  },
  'ChooseUpgradeDeck',
);

export const questionLabelDecoder: JsonDecoder.Decoder<QuestionLabel> = JsonDecoder.object<QuestionLabel>(
  {
    tag: JsonDecoder.isExactly('QuestionLabel'),
    contents: JsonDecoder.lazy(() => JsonDecoder.tuple([JsonDecoder.string, questionDecoder], '[label, question]')),
  },
  'QuestionLabel',
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
    chooseAmountsDecoder,
    choosePaymentAmountsDecoder,
    chooseDynamicCardAmountsDecoder,
    questionLabelDecoder,
  ],
  'Question',
);
