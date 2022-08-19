import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { Source, sourceDecoder } from '@/arkham/types/Source';

export type Question = ChooseOne | ChooseUpToN | ChooseSome | ChooseN | ChooseOneAtATime | ChooseOneFromSource | ChooseUpgradeDeck | ChoosePaymentAmounts | ChooseAmounts | QuestionLabel | Read;

export enum QuestionType {
  CHOOSE_ONE = 'ChooseOne',
  CHOOSE_UP_TO_N = 'ChooseUpToN',
  CHOOSE_SOME = 'ChooseSome',
  CHOOSE_N = 'ChooseN',
  CHOOSE_ONE_AT_A_TIME = 'ChooseOneAtATime',
  CHOOSE_ONE_FROM_SOURCE = 'ChooseOneFromSource',
  CHOOSE_UPGRADE_DECK = 'ChooseUpgradeDeck',
  CHOOSE_PAYMENT_AMOUNTS = 'ChoosePaymentAmounts',
  CHOOSE_AMOUNTS = 'ChooseAmounts',
  QUESTION_LABEL = 'QuestionLabel',
  READ = 'Read',
}

export interface ChooseOne {
  tag: 'ChooseOne';
  choices: Message[];
}

export interface QuestionLabel {
  tag: 'QuestionLabel';
  contents: [string, Question];
}

export interface FlavorText {
  title: string | null;
  body: string[];
}

export interface Read {
  tag: 'Read';
  flavorText: FlavorText;
  readChoices: Message[]
}

export interface ChooseN {
  tag: 'ChooseN';
  amount: number;
  choices: Message[];
}

export interface ChooseSome {
  tag: 'ChooseSome';
  contents: Message[];
}

export interface ChooseUpToN {
  tag: 'ChooseUpToN';
  amount: number;
  choices: Message[];
}

export interface ChooseOneAtATime {
  tag: 'ChooseOneAtATime';
  choices: Message[];
}

export interface ChoosePaymentAmounts {
  tag: 'ChoosePaymentAmounts'
  label: string
  paymentAmountTargetValue: number | null
  paymentAmountChoices: [string, [number, number]][]
}

export interface ChooseAmounts {
  tag: 'ChooseAmounts';
  contents: [string, number, [string, [number, number]][]];
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

// ChoosePaymentAmounts { label :: Text, paymentAmountTargetValue :: (Maybe Int), paymentAmountChoices :: [(InvestigatorId, (Int, Int), msg)] }

export const choosePaymentAmountsDecoder = JsonDecoder.object<ChoosePaymentAmounts>(
  {
    tag: JsonDecoder.isExactly('ChoosePaymentAmounts'),
    label: JsonDecoder.string,
    paymentAmountTargetValue: JsonDecoder.nullable(JsonDecoder.number),
    paymentAmountChoices:
      JsonDecoder.array(
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

export const flavorTextDecoder: JsonDecoder.Decoder<FlavorText> = JsonDecoder.object<FlavorText>(
  {
    title: JsonDecoder.nullable(JsonDecoder.string),
    body: JsonDecoder.array(JsonDecoder.string, 'string[]')
  },
  'FlavorText',
);

export const readDecoder: JsonDecoder.Decoder<Read> = JsonDecoder.object<Read>(
  {
    tag: JsonDecoder.isExactly('Read'),
    flavorText: flavorTextDecoder,
    readChoices: JsonDecoder.array(messageDecoder, 'Message[]')
  },
  'Read',
);

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
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
    amount: JsonDecoder.number,
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseN',
);

export const chooseUpToNDecoder = JsonDecoder.object<ChooseUpToN>(
  {
    tag: JsonDecoder.isExactly('ChooseUpToN'),
    amount: JsonDecoder.number,
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseUpToN',
);

export const chooseOneAtATimeDecoder = JsonDecoder.object<ChooseOneAtATime>(
  {
    tag: JsonDecoder.isExactly('ChooseOneAtATime'),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
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
    questionLabelDecoder,
    readDecoder,
  ],
  'Question',
);
