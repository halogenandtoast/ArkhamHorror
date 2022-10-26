import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';

export type Question = ChooseOne | ChooseUpToN | ChooseSome | ChooseN | ChooseOneAtATime | ChooseUpgradeDeck | ChoosePaymentAmounts | ChooseAmounts | QuestionLabel | Read;

export enum QuestionType {
  CHOOSE_ONE = 'ChooseOne',
  CHOOSE_UP_TO_N = 'ChooseUpToN',
  CHOOSE_SOME = 'ChooseSome',
  CHOOSE_N = 'ChooseN',
  CHOOSE_ONE_AT_A_TIME = 'ChooseOneAtATime',
  CHOOSE_UPGRADE_DECK = 'ChooseUpgradeDeck',
  CHOOSE_PAYMENT_AMOUNTS = 'ChoosePaymentAmounts',
  CHOOSE_AMOUNTS = 'ChooseAmounts',
  QUESTION_LABEL = 'QuestionLabel',
  READ = 'Read',
}

export interface ChooseOne {
  tag: QuestionType.CHOOSE_ONE;
  choices: Message[];
}

export interface QuestionLabel {
  tag: QuestionType.QUESTION_LABEL
  label: string
  question: Question
}

export interface FlavorText {
  title: string | null;
  body: string[];
}

export interface Read {
  tag: QuestionType.READ
  flavorText: FlavorText
  readChoices: Message[]
}

export interface ChooseN {
  tag: QuestionType.CHOOSE_N
  amount: number
  choices: Message[]
}

export interface ChooseSome {
  tag: QuestionType.CHOOSE_SOME
  contents: Message[]
}

export interface ChooseUpToN {
  tag: QuestionType.CHOOSE_UP_TO_N
  amount: number
  choices: Message[]
}

export interface ChooseOneAtATime {
  tag: QuestionType.CHOOSE_ONE_AT_A_TIME
  choices: Message[]
}

export interface ChoosePaymentAmounts {
  tag: QuestionType.CHOOSE_PAYMENT_AMOUNTS
  label: string
  paymentAmountTargetValue: number | null
  paymentAmountChoices: PaymentAmountChoice[]
}

export type AmountTarget = { tag: 'MaxAmountTarget', contents: number } | { tag: 'TotalAmountTarget', contents: number }

export interface ChooseAmounts {
  tag: QuestionType.CHOOSE_AMOUNTS
  label: string
  amountTargetValue: AmountTarget
  amountChoices: AmountChoice[]
}

export interface ChooseUpgradeDeck {
  tag: QuestionType.CHOOSE_UPGRADE_DECK
}

export interface AmountChoice {
  label: string
  minBound: number
  maxBound: number
}

export const amountChoiceDecoder = JsonDecoder.object<AmountChoice>({
  label: JsonDecoder.string,
  minBound: JsonDecoder.number,
  maxBound: JsonDecoder.number,
}, 'AmountChoice')

export const amountTargetDecoder = JsonDecoder.oneOf<AmountTarget>(
  [ JsonDecoder.object({ tag: JsonDecoder.isExactly('MaxAmountTarget'), contents: JsonDecoder.number}, 'MaxAmountTarget')
  , JsonDecoder.object({ tag: JsonDecoder.isExactly('TotalAmountTarget'), contents: JsonDecoder.number}, 'TotalAmountTarget')
  ]
, 'AmountTarget')

export const chooseAmountsDecoder = JsonDecoder.object<ChooseAmounts>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_AMOUNTS),
    label: JsonDecoder.string,
    amountTargetValue: amountTargetDecoder,
    amountChoices: JsonDecoder.array(amountChoiceDecoder, 'AmountChoice[]'),
  }, 'ChooseAmounts',
)

export interface PaymentAmountChoice {
  investigatorId: string
  minBound: number
  maxBound: number
}

export const paymentAmountChoiceDecoder = JsonDecoder.object<PaymentAmountChoice>({
  investigatorId: JsonDecoder.string,
  minBound: JsonDecoder.number,
  maxBound: JsonDecoder.number,
}, 'PaymentAmountChoice')

export const choosePaymentAmountsDecoder = JsonDecoder.object<ChoosePaymentAmounts>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_PAYMENT_AMOUNTS),
    label: JsonDecoder.string,
    paymentAmountTargetValue: JsonDecoder.nullable(JsonDecoder.number),
    paymentAmountChoices: JsonDecoder.array(paymentAmountChoiceDecoder, 'PaymentAmountChoice[]'),
  }, 'ChoosePaymentAmounts',
);

export const chooseUpgradeDeckDecoder = JsonDecoder.object<ChooseUpgradeDeck>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_UPGRADE_DECK),
  },
  'ChooseUpgradeDeck',
);

export const questionLabelDecoder: JsonDecoder.Decoder<QuestionLabel> = JsonDecoder.object<QuestionLabel>(
  {
    tag: JsonDecoder.isExactly(QuestionType.QUESTION_LABEL),
    label: JsonDecoder.string,
    question: JsonDecoder.lazy(() => questionDecoder)
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
    tag: JsonDecoder.isExactly(QuestionType.READ),
    flavorText: flavorTextDecoder,
    readChoices: JsonDecoder.array(messageDecoder, 'Message[]')
  },
  'Read',
);

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_ONE),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOne',
);

export const chooseSomeDecoder = JsonDecoder.object<ChooseSome>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_SOME),
    contents: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseSome',
);

export const chooseNDecoder = JsonDecoder.object<ChooseN>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_N),
    amount: JsonDecoder.number,
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseN',
);

export const chooseUpToNDecoder = JsonDecoder.object<ChooseUpToN>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_UP_TO_N),
    amount: JsonDecoder.number,
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseUpToN',
);

export const chooseOneAtATimeDecoder = JsonDecoder.object<ChooseOneAtATime>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_ONE_AT_A_TIME),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOneAtATime',
);

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [
    chooseOneDecoder,
    chooseNDecoder,
    chooseSomeDecoder,
    chooseUpToNDecoder,
    chooseOneAtATimeDecoder,
    chooseUpgradeDeckDecoder,
    chooseAmountsDecoder,
    choosePaymentAmountsDecoder,
    questionLabelDecoder,
    readDecoder,
  ],
  'Question',
);
