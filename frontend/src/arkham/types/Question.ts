import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { Source, sourceDecoder } from '@/arkham/types/Source';

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
  tag: 'ChooseOne';
  choices: Message[];
}

export interface QuestionLabel {
  tag: 'QuestionLabel'
  label: string
  question: Question
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
  paymentAmountChoices: PaymentAmountChoice[]
}

export interface ChooseAmounts {
  tag: 'ChooseAmounts'
  label: string
  amountTargetValue: number
  amountChoices: AmountChoice[]
}

export interface ChooseUpgradeDeck {
  tag: 'ChooseUpgradeDeck';
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

export const chooseAmountsDecoder = JsonDecoder.object<ChooseAmounts>(
  {
    tag: JsonDecoder.isExactly('ChooseAmounts'),
    label: JsonDecoder.string,
    amountTargetValue: JsonDecoder.number,
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
    tag: JsonDecoder.isExactly('ChoosePaymentAmounts'),
    label: JsonDecoder.string,
    paymentAmountTargetValue: JsonDecoder.nullable(JsonDecoder.number),
    paymentAmountChoices: JsonDecoder.array(paymentAmountChoiceDecoder, 'PaymentAmountChoice[]'),
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
