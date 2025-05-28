import * as JsonDecoder from 'ts.data.json';
import { Message, messageDecoder } from '@/arkham/types/Message';
import { FlavorText, flavorTextDecoder } from '@/arkham/types/FlavorText';

export type Question = ChooseOne | ChooseUpToN | ChooseSome | ChooseSome1 | ChooseN | ChooseOneAtATime | ChooseOneAtATimeWithAuto | ChooseDeck | ChooseUpgradeDeck | ChoosePaymentAmounts | ChooseAmounts | QuestionLabel | Read | PickSupplies | DropDown | PickScenarioSettings | PickCampaignSettings | ChooseOneFromEach;

export enum QuestionType {
  CHOOSE_ONE = 'ChooseOne',
  PLAYER_WINDOW_CHOOSE_ONE = 'PlayerWindowChooseOne',
  CHOOSE_ONE_FROM_EACH = 'ChooseOneFromEach',
  CHOOSE_UP_TO_N = 'ChooseUpToN',
  CHOOSE_SOME = 'ChooseSome',
  CHOOSE_SOME_1 = 'ChooseSome1',
  CHOOSE_N = 'ChooseN',
  CHOOSE_ONE_AT_A_TIME = 'ChooseOneAtATime',
  CHOOSE_ONE_AT_A_TIME_WITH_AUTO = 'ChooseOneAtATimeWithAuto',
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

export type PickScenarioSettings = {
  tag: QuestionType.PICK_SCENARIO_SETTINGS;
}

export type PickCampaignSettings = {
  tag: QuestionType.PICK_CAMPAIGN_SETTINGS;
}

export type ChooseOne = {
  tag: QuestionType.CHOOSE_ONE;
  choices: Message[];
}

// The backend represents this as a nest list, but we flatten it and pass the flattened index
export type ChooseOneFromEach = {
  tag: QuestionType.CHOOSE_ONE_FROM_EACH;
  choices: Message[];
}

export type QuestionLabel = {
  tag: QuestionType.QUESTION_LABEL
  card: string | null
  label: string
  question: Question
}

export type Read = {
  tag: QuestionType.READ
  flavorText: FlavorText
  readChoices: ReadChoices
  readCards: string[] | null;
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
  JsonDecoder.literal('Provisions'),
  JsonDecoder.literal('Medicine'),
  JsonDecoder.literal('Rope'),
  JsonDecoder.literal('Blanket'),
  JsonDecoder.literal('Canteen'),
  JsonDecoder.literal('Torches'),
  JsonDecoder.literal('Compass'),
  JsonDecoder.literal('Map'),
  JsonDecoder.literal('Binoculars'),
  JsonDecoder.literal('Chalk'),
  JsonDecoder.literal('Pendant'),
  JsonDecoder.literal('Gasoline'),
  JsonDecoder.literal('Pocketknife'),
  JsonDecoder.literal('Pickaxe')
], 'Supply')

export type PickSupplies = {
  tag: QuestionType.PICK_SUPPLIES
  pointsRemaining: number
  chosenSupplies: Supply[]
  choices: Message[]
}

export type DropDown = {
  tag: QuestionType.DROP_DOWN
  options: string[]
}

export type ChooseN = {
  tag: QuestionType.CHOOSE_N
  amount: number
  choices: Message[]
}

export type ChooseSome = {
  tag: QuestionType.CHOOSE_SOME
  choices: Message[]
}

export type ChooseSome1 = {
  tag: QuestionType.CHOOSE_SOME_1
  choices: Message[]
}

export type ChooseUpToN = {
  tag: QuestionType.CHOOSE_UP_TO_N
  amount: number
  choices: Message[]
}

export type ChooseOneAtATime = {
  tag: QuestionType.CHOOSE_ONE_AT_A_TIME
  choices: Message[]
}

export type ChooseOneAtATimeWithAuto = {
  tag: QuestionType.CHOOSE_ONE_AT_A_TIME_WITH_AUTO
  label: string
  choices: Message[]
}

export type ChoosePaymentAmounts = {
  tag: QuestionType.CHOOSE_PAYMENT_AMOUNTS
  label: string
  paymentAmountTargetValue: AmountTarget | null
  paymentAmountChoices: PaymentAmountChoice[]
}

export type AmountTarget
  = { tag: 'MaxAmountTarget', contents: number }
  | { tag: 'TotalAmountTarget', contents: number }
  | { tag: "MinAmountTarget", contents: number }
  | { tag: 'AmountOneOf', contents: number[] }

export type ChooseAmounts = {
  tag: QuestionType.CHOOSE_AMOUNTS
  label: string
  amountTargetValue: AmountTarget
  amountChoices: AmountChoice[]
}

export type ChooseUpgradeDeck = {
  tag: QuestionType.CHOOSE_UPGRADE_DECK
}

export type ChooseDeck = {
  tag: QuestionType.CHOOSE_DECK
}

export type AmountChoice = {
  choiceId: string
  label: string
  minBound: number
  maxBound: number
}

export const amountChoiceDecoder = JsonDecoder.object<AmountChoice>({
  choiceId: JsonDecoder.string(),
  label: JsonDecoder.string(),
  minBound: JsonDecoder.number(),
  maxBound: JsonDecoder.number(),
}, 'AmountChoice')

export const amountTargetDecoder = JsonDecoder.oneOf<AmountTarget>(
  [ JsonDecoder.object({ tag: JsonDecoder.literal('MaxAmountTarget'), contents: JsonDecoder.number()}, 'MaxAmountTarget')
  , JsonDecoder.object({ tag: JsonDecoder.literal('MinAmountTarget'), contents: JsonDecoder.number()}, 'MinAmountTarget')
  , JsonDecoder.object({ tag: JsonDecoder.literal('TotalAmountTarget'), contents: JsonDecoder.number()}, 'TotalAmountTarget')
  , JsonDecoder.object({ tag: JsonDecoder.literal('AmountOneOf'), contents: JsonDecoder.array(JsonDecoder.number(), 'number[]')}, 'AmountOneOf')
  ]
, 'AmountTarget')

export const chooseAmountsDecoder = JsonDecoder.object<ChooseAmounts>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_AMOUNTS),
    label: JsonDecoder.string(),
    amountTargetValue: amountTargetDecoder,
    amountChoices: JsonDecoder.array(amountChoiceDecoder, 'AmountChoice[]'),
  }, 'ChooseAmounts',
)

export type PaymentAmountChoice = {
  choiceId: string
  investigatorId: string
  minBound: number
  maxBound: number
  title: string
}

export const paymentAmountChoiceDecoder = JsonDecoder.object<PaymentAmountChoice>({
  choiceId: JsonDecoder.string(),
  investigatorId: JsonDecoder.string(),
  minBound: JsonDecoder.number(),
  maxBound: JsonDecoder.number(),
  title: JsonDecoder.string(),
}, 'PaymentAmountChoice')

export const choosePaymentAmountsDecoder = JsonDecoder.object<ChoosePaymentAmounts>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_PAYMENT_AMOUNTS),
    label: JsonDecoder.string(),
    paymentAmountTargetValue: JsonDecoder.nullable(amountTargetDecoder),
    paymentAmountChoices: JsonDecoder.array(paymentAmountChoiceDecoder, 'PaymentAmountChoice[]'),
  }, 'ChoosePaymentAmounts',
);

export const chooseUpgradeDeckDecoder = JsonDecoder.object<ChooseUpgradeDeck>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_UPGRADE_DECK),
  },
  'ChooseUpgradeDeck',
);

export const chooseDeckDecoder = JsonDecoder.object<ChooseDeck>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_DECK),
  },
  'ChooseDeck',
);

export const pickScenarioSettingsDecoder = JsonDecoder.object<PickScenarioSettings>(
  {
    tag: JsonDecoder.literal(QuestionType.PICK_SCENARIO_SETTINGS),
  },
  'PickScenarioSettings',
);

export const pickCampaignSettingsDecoder = JsonDecoder.object<PickCampaignSettings>(
  {
    tag: JsonDecoder.literal(QuestionType.PICK_CAMPAIGN_SETTINGS),
  },
  'PickCampaignSettings',
);

export const questionLabelDecoder: JsonDecoder.Decoder<QuestionLabel> = JsonDecoder.object<QuestionLabel>(
  {
    tag: JsonDecoder.literal(QuestionType.QUESTION_LABEL),
    label: JsonDecoder.string(),
    card: JsonDecoder.nullable(JsonDecoder.string()),
    question: JsonDecoder.lazy(() => questionDecoder)
  },
  'QuestionLabel',
);


export type ReadChoices
  = { tag: "BasicReadChoices", contents: Message[] }
  | { tag: "LeadInvestigatorMustDecide", contents: Message [] }


export const readChoicesDecoder: JsonDecoder.Decoder<ReadChoices> = JsonDecoder.oneOf<ReadChoices>( [
  JsonDecoder.object({
    tag: JsonDecoder.literal('BasicReadChoices'),
    contents: JsonDecoder.array(messageDecoder, 'Message[]')
  }, 'BasicReadChoices'),
  JsonDecoder.object({
    tag: JsonDecoder.literal('BasicReadChoicesN'),
    contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.array(messageDecoder, 'Message[]')], 'contents')
  }, 'BasicReadChoicesN').map(({ contents }) => ({ tag: 'BasicReadChoices', contents: contents[1] })),
  JsonDecoder.object({
    tag: JsonDecoder.literal('BasicReadChoicesUpToN'),
    contents: JsonDecoder.tuple([JsonDecoder.number(), JsonDecoder.array(messageDecoder, 'Message[]')], 'contents')
  }, 'BasicReadChoicesUpToN').map(({ contents }) => ({ tag: 'BasicReadChoices', contents: contents[1] })),
  JsonDecoder.object({
    tag: JsonDecoder.literal('LeadInvestigatorMustDecide'),
    contents: JsonDecoder.array(messageDecoder, 'Message[]')
    }, 'LeadInvestigatorMustDecide')
], 'ReadChoices');

export const readDecoder: JsonDecoder.Decoder<Read> = JsonDecoder.object<Read>(
  {
    tag: JsonDecoder.literal(QuestionType.READ),
    flavorText: flavorTextDecoder,
    readChoices: readChoicesDecoder,
    readCards: JsonDecoder.nullable(JsonDecoder.array(JsonDecoder.string(), 'CardCodes[]'))
  },
  'Read',
);

export const pickSuppliesDecoder = JsonDecoder.object<PickSupplies>(
  {
    tag: JsonDecoder.literal(QuestionType.PICK_SUPPLIES),
    pointsRemaining: JsonDecoder.number(),
    chosenSupplies: JsonDecoder.array<Supply>(supplyDecoder, 'Supply[]'),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'PickSupplies',
);

export const dropDownDecoder = JsonDecoder.object<DropDown>(
  {
    tag: JsonDecoder.literal(QuestionType.DROP_DOWN),
    options: JsonDecoder.array(JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.succeed()], '[string, message]').map(([s,]) => s), 'string[]') //eslint-disable-line
  },
  'PickSupplies',
);

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.oneOf(
        [JsonDecoder.literal(QuestionType.CHOOSE_ONE)
        , JsonDecoder.literal(QuestionType.PLAYER_WINDOW_CHOOSE_ONE).map(() => QuestionType.CHOOSE_ONE)
        ], "ChooseOne.tag"),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOne',
);

export const chooseOneFromEachDecoder = JsonDecoder.object<ChooseOneFromEach>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_ONE_FROM_EACH),
    choices: JsonDecoder.array<Message[]>(JsonDecoder.array<Message>(messageDecoder, 'Message[]'), 'Message[][]').map(xs => xs.flat()),
  },
  'ChooseOneFromEach',
);

export const chooseSomeDecoder = JsonDecoder.object<ChooseSome>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_SOME),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseSome',
);

export const chooseSome1Decoder = JsonDecoder.object<ChooseSome1>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_SOME_1),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseSome1',
);

export const chooseNDecoder = JsonDecoder.object<ChooseN>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_N),
    amount: JsonDecoder.number(),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseN',
);

export const chooseUpToNDecoder = JsonDecoder.object<ChooseUpToN>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_UP_TO_N),
    amount: JsonDecoder.number(),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseUpToN',
);

export const chooseOneAtATimeDecoder = JsonDecoder.object<ChooseOneAtATime>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_ONE_AT_A_TIME),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOneAtATime',
);

export const chooseOneAtATimeWithAutoDecoder = JsonDecoder.object<ChooseOneAtATimeWithAuto>(
  {
    tag: JsonDecoder.literal(QuestionType.CHOOSE_ONE_AT_A_TIME_WITH_AUTO),
    label: JsonDecoder.string(),
    choices: JsonDecoder.array<Message>(messageDecoder, 'Message[]'),
  },
  'ChooseOneAtATimeWithAuto',
);

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [
    chooseOneDecoder,
    chooseOneFromEachDecoder,
    chooseNDecoder,
    chooseSomeDecoder,
    chooseSome1Decoder,
    chooseUpToNDecoder,
    chooseOneAtATimeDecoder,
    chooseOneAtATimeWithAutoDecoder,
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
    JsonDecoder.succeed().flatMap((f) => {
      return JsonDecoder.fail(f)
    })
  ],
  'Question',
);
