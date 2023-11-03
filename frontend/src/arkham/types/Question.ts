import { JsonDecoder } from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';

export type Question = ChooseOne | ChooseUpToN | ChooseSome | ChooseSome1 | ChooseN | ChooseOneAtATime | ChooseDeck | ChooseUpgradeDeck | ChoosePaymentAmounts | ChooseAmounts | QuestionLabel | Read | PickSupplies | DropDown | PickScenarioSettings | PickCampaignSettings;

export enum QuestionType {
  CHOOSE_ONE = 'ChooseOne',
  CHOOSE_UP_TO_N = 'ChooseUpToN',
  CHOOSE_SOME = 'ChooseSome',
  CHOOSE_SOME_1 = 'ChooseSome1',
  CHOOSE_N = 'ChooseN',
  CHOOSE_ONE_AT_A_TIME = 'ChooseOneAtATime',
  CHOOSE_UPGRADE_DECK = 'ChooseUpgradeDeck',
  CHOOSE_DECK = 'ChooseDeck',
  CHOOSE_PAYMENT_AMOUNTS = 'ChoosePaymentAmounts',
  CHOOSE_AMOUNTS = 'ChooseAmounts',
  QUESTION_LABEL = 'QuestionLabel',
  READ = 'Read',
  PICK_SUPPLIES = 'PickSupplies',
  DROP_DOWN = 'DropDown',
  PICK_SCENARIO_SETTINGS = 'PickScenarioSettings',
  PICK_CAMPAIGN_SETTINGS = 'PickCampaignSettings',
}

export interface PickScenarioSettings {
  tag: QuestionType.PICK_SCENARIO_SETTINGS;
}

export interface PickCampaignSettings {
  tag: QuestionType.PICK_CAMPAIGN_SETTINGS;
}

export interface ChooseOne {
  tag: QuestionType.CHOOSE_ONE;
  choices: Message[];
}

export interface QuestionLabel {
  tag: QuestionType.QUESTION_LABEL
  card: string | null
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

type Supply
  = 'Provisions'
  | 'Medicine'
  | 'Rope'
  | 'Blanket'
  | 'Canteen'
  | 'Torches'
  | 'Compass'
  | 'Map'
  | 'Binoculars'
  | 'Chalk'
  | 'Pendant'
  | 'Gasoline'
  | 'Pocketknife'
  | 'Pickaxe'

export const supplyDecoder = JsonDecoder.oneOf<Supply>([
  JsonDecoder.isExactly('Provisions'),
  JsonDecoder.isExactly('Medicine'),
  JsonDecoder.isExactly('Rope'),
  JsonDecoder.isExactly('Blanket'),
  JsonDecoder.isExactly('Canteen'),
  JsonDecoder.isExactly('Torches'),
  JsonDecoder.isExactly('Compass'),
  JsonDecoder.isExactly('Map'),
  JsonDecoder.isExactly('Binoculars'),
  JsonDecoder.isExactly('Chalk'),
  JsonDecoder.isExactly('Pendant'),
  JsonDecoder.isExactly('Gasoline'),
  JsonDecoder.isExactly('Pocketknife'),
  JsonDecoder.isExactly('Pickaxe')
], 'Supply')

export interface PickSupplies {
  tag: QuestionType.PICK_SUPPLIES
  pointsRemaining: number
  chosenSupplies: Supply[]
  choices: Message[]
}

export interface DropDown {
  tag: QuestionType.DROP_DOWN
  options: string[]
}

export interface ChooseN {
  tag: QuestionType.CHOOSE_N
  amount: number
  choices: Message[]
}

export interface ChooseSome {
  tag: QuestionType.CHOOSE_SOME
  choices: Message[]
}

export interface ChooseSome1 {
  tag: QuestionType.CHOOSE_SOME_1
  choices: Message[]
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

export type AmountTarget = { tag: 'MaxAmountTarget', contents: number } | { tag: 'TotalAmountTarget', contents: number } | { tag: "MinAmountTarget", contents: number }

export interface ChooseAmounts {
  tag: QuestionType.CHOOSE_AMOUNTS
  label: string
  amountTargetValue: AmountTarget
  amountChoices: AmountChoice[]
}

export interface ChooseUpgradeDeck {
  tag: QuestionType.CHOOSE_UPGRADE_DECK
}

export interface ChooseDeck {
  tag: QuestionType.CHOOSE_DECK
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
  , JsonDecoder.object({ tag: JsonDecoder.isExactly('MinAmountTarget'), contents: JsonDecoder.number}, 'MinAmountTarget')
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
  title: string
}

export const paymentAmountChoiceDecoder = JsonDecoder.object<PaymentAmountChoice>({
  investigatorId: JsonDecoder.string,
  minBound: JsonDecoder.number,
  maxBound: JsonDecoder.number,
  title: JsonDecoder.string,
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

export const chooseDeckDecoder = JsonDecoder.object<ChooseDeck>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_DECK),
  },
  'ChooseDeck',
);

export const pickScenarioSettingsDecoder = JsonDecoder.object<PickScenarioSettings>(
  {
    tag: JsonDecoder.isExactly(QuestionType.PICK_SCENARIO_SETTINGS),
  },
  'PickScenarioSettings',
);

export const pickCampaignSettingsDecoder = JsonDecoder.object<PickCampaignSettings>(
  {
    tag: JsonDecoder.isExactly(QuestionType.PICK_CAMPAIGN_SETTINGS),
  },
  'PickCampaignSettings',
);

export const questionLabelDecoder: JsonDecoder.Decoder<QuestionLabel> = JsonDecoder.object<QuestionLabel>(
  {
    tag: JsonDecoder.isExactly(QuestionType.QUESTION_LABEL),
    label: JsonDecoder.string,
    card: JsonDecoder.nullable(JsonDecoder.string),
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

export const pickSuppliesDecoder = JsonDecoder.object<PickSupplies>(
  {
    tag: JsonDecoder.isExactly(QuestionType.PICK_SUPPLIES),
    pointsRemaining: JsonDecoder.number,
    chosenSupplies: JsonDecoder.array<Supply>(supplyDecoder, 'Supply[]'),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'PickSupplies',
);

export const dropDownDecoder = JsonDecoder.object<DropDown>(
  {
    tag: JsonDecoder.isExactly(QuestionType.DROP_DOWN),
    options: JsonDecoder.array(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.succeed], '[string, message]').map(([s,]) => s), 'string[]') //eslint-disable-line
  },
  'PickSupplies',
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
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseSome',
);

export const chooseSome1Decoder = JsonDecoder.object<ChooseSome1>(
  {
    tag: JsonDecoder.isExactly(QuestionType.CHOOSE_SOME_1),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseSome1',
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
    chooseSome1Decoder,
    chooseUpToNDecoder,
    chooseOneAtATimeDecoder,
    chooseUpgradeDeckDecoder,
    chooseDeckDecoder,
    chooseAmountsDecoder,
    choosePaymentAmountsDecoder,
    questionLabelDecoder,
    readDecoder,
    pickSuppliesDecoder,
    dropDownDecoder,
    pickScenarioSettingsDecoder,
    pickCampaignSettingsDecoder,
    JsonDecoder.succeed.chain((f) => {
      return JsonDecoder.fail(f)
    })
  ],
  'Question',
);
