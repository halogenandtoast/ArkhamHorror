import { JsonDecoder } from 'ts.data.json';
import {
  Card,
  cardDecoder,
  CardContents,
  cardContentsDecoder,
} from '@/arkham/types/Card';
import { ChaosBag, chaosBagDecoder } from '@/arkham/types/ChaosBag';
import { logContentsDecoder } from '@/arkham/types/Log';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
import type { LogContents } from '@/arkham/types/Campaign';
import { Difficulty, difficultyDecoder } from '@/arkham/types/Difficulty';
import { Tokens, tokensDecoder } from '@/arkham/types/Token';
import { TarotCard, tarotCardDecoder, tarotScopeDecoder } from '@/arkham/types/TarotCard';

export type ScenarioName = {
  title: string;
  subtitle: string | null;
}

export const scenarioNameDecoder = JsonDecoder.object<ScenarioName>(
  {
    title: JsonDecoder.string,
    subtitle: JsonDecoder.nullable(JsonDecoder.string),
  },
  'ScenarioName'
);

export type ScenarioDeck = {
  tag: string;
  deckSize: number;
}

export type ScenarioDetails = {
  id: string;
  difficulty: Difficulty;
  name: ScenarioName;
}

export type Scenario = {
  name: ScenarioName;
  id: string;
  reference: string;
  difficulty: Difficulty;
  locationLayout: string[] | null;
  usesGrid: boolean;
  decksLayout: string[];
  decks: [string, Card[]][];
  cardsUnderScenarioReference: Card[];
  cardsUnderAgendaDeck: Card[];
  cardsUnderActDeck: Card[];
  cardsNextToActDeck: Card[];
  cardsNextToAgendaDeck: Card[];
  setAsideCards: Card[];
  setAsideKeys: ArkhamKey[];
  chaosBag: ChaosBag;
  discard: CardContents[];
  victoryDisplay: Card[];
  standaloneCampaignLog: LogContents | null;
  tokens: Tokens;
  counts: Record<string, number>; // eslint-disable-line
  encounterDecks: Record<string, [CardContents[], CardContents[]]>;
  hasEncounterDeck: boolean;
  tarotCards: TarotCard[];
}

export const scenarioDeckDecoder = JsonDecoder.object<ScenarioDeck>({
  tag: JsonDecoder.string,
  deckSize: JsonDecoder.array<Card>(cardDecoder, 'Card[]').map(cards => cards.length),
}, 'ScenarioDeck', { deckSize: 'contents' });

export const scenarioDetailsDecoder = JsonDecoder.object<ScenarioDetails>({
  id: JsonDecoder.string,
  difficulty: difficultyDecoder,
  name: scenarioNameDecoder,
}, 'ScenarioDetails');

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string,
  reference: JsonDecoder.string,
  difficulty: difficultyDecoder,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
  usesGrid: JsonDecoder.boolean,
  decksLayout: JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  cardsUnderScenarioReference: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderActDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathActCards'),
  cardsNextToActDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToActDeck'),
  cardsNextToAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToAgendaDeck'),
  setAsideKeys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
  chaosBag: chaosBagDecoder,
  discard: JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]'),
  victoryDisplay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  standaloneCampaignLog: logContentsDecoder,
  tokens: tokensDecoder,
  hasEncounterDeck: JsonDecoder.boolean,
  // tarotCards: JsonDecoder.array<TarotCard>(tarotCardDecoder, 'TarotCard[]'),
  tarotCards: JsonDecoder.
    array(
      JsonDecoder.tuple([tarotScopeDecoder, JsonDecoder.array(tarotCardDecoder, 'TarotCard[]')], '[TarotScope, TarotCard[]]'),
      '[TarotScope, TarotCard[]][]'
    ).map(res => res.reduce<TarotCard[]>((acc, [k, vs]) => [...acc, ...vs.map(v => ({ ...v, scope: k }))], [])),
  counts:
    JsonDecoder.array<[string, number]>(
      JsonDecoder.oneOf([
        JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.number], '[string, number]'),
        JsonDecoder.tuple([JsonDecoder.object(
            { tag: JsonDecoder.string }
            , 'count with arg'
        ), JsonDecoder.number], '[string, number]').map(([a, b]) => [a.tag, b]),
      ], 'counts'),
      '[string, number][]'
    ).
    map<Record<string, number>>(res => {
      return res.reduce<Record<string, number>>((acc, [k, v]) => {
        acc[k] = v
        return acc
      }, {})
    }),
  encounterDecks: JsonDecoder.array<[string, [CardContents[], CardContents[]]]>(
    JsonDecoder.tuple([
      JsonDecoder.string,
      JsonDecoder.tuple([
        JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]'),
        JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]')
      ], '[[EncounterCardContents[], EncounterCardContents[]]'),
    ], '[string, [EncounterCardContents[], EncounterCardContents[]]'),
    '[string, [EncounterCardContents[], EncounterCardContents[]]][]').map<Record<string, [CardContents[], CardContents[]]>>(res => {
      return res.reduce<Record<string, [CardContents[], CardContents[]]>>((acc, [k, v]) => {
        acc[k] = v
        return acc
      }, {})
    }),

}, 'Scenario');


//  {
//    "id": "01120",
//    "name": "The Midnight Masks",
//    "campaign": "01",
//    "settings": [
//      { "key": "LitaWasForcedToFindOthersToHelpHerCause", "type": "ToggleKey", "content": false },
//      { "key": "HouseStatus", "keys": ["YourHouseHasBurnedToTheGround", "YourHouseIsStillStanding"], "type": "PickKey", "content": "YourHouseHasBurnedToTheGround" },
//      { "key": "GhoulPriestIsStillAlive", "type": "ToggleKey", "content": false },
//      { "key": "AddLitaChantler", "type": "ToggleOption", "content": false }
//    ]
//  },
//  {
//    "id": "01142",
//    "name": "The Devourer Below",
//    "campaign": "01",
//    "settings": [
//      { "key": "ItIsPastMidnight", "type": "ToggleKey", "content": false },
//      { "key": "GhoulPriestIsStillAlive", "type": "ToggleKey", "content": false },
//      { "key": "CultistsWhoGotAway", "recordable": "RecordableCardCode", "type": "ToggleCrossedOut", "content": [
//        { "label": "\"Wolf-Man\" Drew", "key": "01137", "content": false },
//        { "label": "Herman Collins", "key": "01138", "content": false },
//        { "label": "Peter Warren", "key": "01139", "content": false },
//        { "label": "Victoria Devereux", "key": "01140", "content": false },
//        { "label": "Ruth Turner", "key": "01141", "content": false },
//        { "label": "The Masked Hunter", "key": "01121b", "content": false }
//      ] },
//      { "key": "AddLitaChantler", "type": "ToggleOption", "content": false }
//    ]
//  },
//
//  { "id": "02041", "name": "Extracurricular Activity", "campaign": "02" },
//  { "id": "02062", "name": "The House Always Wins", "campaign": "02" },
//  { "id": "02118", "name": "The Miskatonic Museum", "campaign": "02" },
//  { "id": "02159", "name": "The Essex County Express", "campaign": "02" },
//  { "id": "02195", "name": "Blood on the Altar", "campaign": "02" },
//  { "id": "02236", "name": "Undimensioned and Unseen", "campaign": "02" },
//  { "id": "02274", "name": "Where Doom Awaits", "campaign": "02" },
//  { "id": "02311", "name": "Lost in Time and Space", "campaign": "02" },
//
//  { "id": "03043", "name": "Curtain Call", "campaign": "03" },
//  { "id": "03061", "name": "The Last King", "campaign": "03" },
//  { "id": "03120", "name": "Echoes of the Past", "campaign": "03" },
//  { "id": "03159", "name": "The Unspeakable Oath", "campaign": "03" },
//  { "id": "03200", "name": "A Phantom of Truth", "campaign": "03" },
//  { "id": "03240", "name": "The Pallid Mask", "campaign": "03" },
//  { "id": "03274", "name": "Black Stars Rise", "campaign": "03" },
//  { "id": "03316", "name": "Dim Carcosa", "campaign": "03" },
//
//  { "id": "04043", "name": "The Untamed Wilds", "campaign": "04" },
//  { "id": "04054", "name": "The Doom of Eztli", "campaign": "04" },
//  { "id": "04113", "name": "Threads of Fate", "campaign": "04" },
//  { "id": "04161", "name": "The Boundary Beyond", "campaign": "04" },
//  { "id": "04205", "name": "Heart of the Elders", "campaign": "04" },
//  { "id": "04237", "name": "The City of Archives", "campaign": "04" },
//  { "id": "04277", "name": "The Depths of Yoth", "campaign": "04" },
//  { "id": "04314", "name": "Shattered Aeons", "campaign": "04" },
//
//  { "id": "05043", "name": "Disappearance at the Twilight Estate", "campaign": "05" },
//  { "id": "05050", "name": "The Witching Hour", "campaign": "05" },
//  { "id": "05065", "name": "At Death's Doorstep", "campaign": "05" },
//  { "id": "05120", "name": "The Secret Name", "campaign": "05" },
//  { "id": "05161", "name": "The Wages of Sin", "campaign": "05" },
//  { "id": "05197", "name": "For the Greater Good", "campaign": "05" },
//  {
//    "id": "05238",
//    "name": "Union and Disillusion",
//    "campaign": "05",
//    "settings": [
//      { "key": "MissingPersons", "recordable": "RecordableCardCode", "type": "ToggleCrossedOut", "content": [
//        { "label": "Gaveriella Mizrah", "key": "05046", "content": true },
//        { "label": "Jerome Davids", "key": "05047", "content": true },
//        { "label": "Valentino Rivas", "key": "05048", "content": true },
//        { "label": "Penny White", "key": "05049", "content": true }
//      ] },
//      {
//        "type": "ChooseRecord",
//        "recordable": "RecordableCardCode",
//        "label": "Gavriella Mizrah",
//        "key": "05046",
//        "selected": null,
//        "content": [
//          {"key": "WasTakenByTheWatcher"},
//          {"key": "WasClaimedBySpecters"},
//          {"key": "DisappearedIntoTheMist"},
//          {"key": "WasPulledIntoTheSpectralRealm"}
//        ],
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05046" }
//        ]
//      },
//      {
//        "key": "TheInvestigatorsAreOnGavriella'sTrail",
//        "type": "ToggleKey",
//        "content": false,
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05046" }
//        ]
//      },
//      {
//        "type": "ChooseRecord",
//        "recordable": "RecordableCardCode",
//        "label": "Jerome Davids",
//        "key": "05047",
//        "selected": null,
//        "content": [
//          {"key": "WasTakenByTheWatcher"},
//          {"key": "WasClaimedBySpecters"},
//          {"key": "DisappearedIntoTheMist"},
//          {"key": "WasPulledIntoTheSpectralRealm"}
//        ],
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05047" }
//        ]
//      },
//      {
//        "key": "TheInvestigatorsAreOnJerome'sTrail",
//        "type": "ToggleKey",
//        "content": false,
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05047" }
//        ]
//      },
//      {
//        "type": "ChooseRecord",
//        "recordable": "RecordableCardCode",
//        "label": "Valentino Rivas",
//        "key": "05048",
//        "selected": null,
//        "content": [
//          {"key": "WasTakenByTheWatcher"},
//          {"key": "WasClaimedBySpecters"},
//          {"key": "DisappearedIntoTheMist"},
//          {"key": "WasPulledIntoTheSpectralRealm"}
//        ],
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05048" }
//        ]
//      },
//      {
//        "key": "TheInvestigatorsAreOnValentino'sTrail",
//        "type": "ToggleKey",
//        "content": false,
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05048" }
//        ]
//      },
//      {
//        "type": "ChooseRecord",
//        "recordable": "RecordableCardCode",
//        "label": "Penny White",
//        "key": "05049",
//        "selected": null,
//        "content": [
//          {"key": "WasTakenByTheWatcher"},
//          {"key": "WasClaimedBySpecters"},
//          {"key": "DisappearedIntoTheMist"},
//          {"key": "WasPulledIntoTheSpectralRealm"}
//        ],
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05049" }
//        ]
//      },
//      { "key": "TheInvestigatorsWereInductedIntoTheInnerCircle", "type": "ToggleKey", "content": false },
//      { "key": "TheInvestigatorsAreDeceivingTheLodge", "type": "ToggleKey", "content": false },
//      { "key": "JosefIsAliveAndWell", "type": "ToggleKey", "content": true },
//      { "key": "TheWitches'SpellWasCast", "type": "ToggleKey", "content": false },
//      {
//        "key": "TheInvestigatorsAreOnPenny'sTrail",
//        "type": "ToggleKey",
//        "content": false,
//        "ifRecorded": [
//          {"type": "inSet", "key": "MissingPersons", "content": "05049" }
//        ]
//      }
//    ]
//  },
//  {
//    "id": "05284",
//    "name": "In the Clutches of Chaos",
//    "campaign": "05",
//    "settings": []
//  },
//  {
//    "id": "05325",
//    "name": "Before the Black Throne",
//    "campaign": "05",
//    "settings": [
//        {
//          "key": "ThePathWindsBeforeYou",
//          "type": "ChooseNum",
//          "max": 6,
//          "content": 0
//        }
//    ]
//  },
//  {
//    "id": "06039",
//    "name": "Beyond the Gates of Sleep",
//    "campaign": "06",
//    "settings": []
//  },
//  {
//    "id": "06063",
//    "name": "Waking Nightmare",
//    "campaign": "06",
//    "settings": []
//  },
//  {
//    "id": "06119",
//    "name": "The Search for Kadath",
//    "campaign": "06",
//    "settings": [
//      { "key": "TheInvestigatorsParleyedWithTheZoogs", "type": "ToggleKey", "content": false }
//    ]
//  },
//  {
//    "id": "06168",
//    "name": "A Thousand Shapes of Horror",
//    "campaign": "06",
//    "settings": []
//  },
//  {
//    "id": "06206",
//    "name": "Dark Side of the Moon",
//    "campaign": "06",
//    "settings": []
//  },
//  {
//    "id": "06247",
//    "name": "Point of No Return",
//    "campaign": "06",
//    "settings": []
//  },
//  {
//    "id": "06286",
//    "name": "Where the Gods Dwell",
//    "campaign": "06",
//    "settings": []
//  },
//  {
//    "id": "06333",
//    "name": "Weaver of the Cosmos",
//    "campaign": "06",
//    "settings": []
//  }
export function scenarioToI18n(scenario: Scenario): string {
  switch (scenario.id) {
    case "c01104": return "nightOfTheZealot.theGathering"
    case "c01120": return "nightOfTheZealot.theMidnightMasks"
    case "c01142": return "nightOfTheZealot.theDevourerBelow"
    case "c02041": return "theDunwichLegacy.extracurricularActivity"
    case "c02062": return "theDunwichLegacy.theHouseAlwaysWins"
    case "c02118": return "theDunwichLegacy.theMiskatonicMuseum"
    case "c02159": return "theDunwichLegacy.theEssexCountyExpress"
    case "c02195": return "theDunwichLegacy.bloodOnTheAltar"
    case "c02236": return "theDunwichLegacy.undimensionedAndUnseen"
    case "c02274": return "theDunwichLegacy.whereDoomAwaits"
    case "c02311": return "theDunwichLegacy.lostInTimeAndSpace"
    case "c03043": return "thePathToCarcosa.curtainCall"
    case "c03061": return "thePathToCarcosa.theLastKing"
    case "c03120": return "thePathToCarcosa.echoesOfThePast"
    case "c03159": return "thePathToCarcosa.theUnspeakableOath"
    case "c03200": return "thePathToCarcosa.aPhantomOfTruth"
    case "c03240": return "thePathToCarcosa.thePallidMask"
    case "c03274": return "thePathToCarcosa.blackStarsRise"
    case "c03316": return "thePathToCarcosa.dimCarcosa"
    case "c04043": return "theForgottenAge.theUntamedWilds"
    case "c04054": return "theForgottenAge.theDoomOfEztli"
    case "c04113": return "theForgottenAge.threadsOfFate"
    case "c04161": return "theForgottenAge.theBoundaryBeyond"
    case "c04205": return "theForgottenAge.heartOfTheElders"
    case "c04237": return "theForgottenAge.theCityOfArchives"
    case "c04277": return "theForgottenAge.theDepthsOfYoth"
    case "c04314": return "theForgottenAge.shatteredAeons"
    case "c05043": return "theCircleUndone.disappearanceAtTheTwilightEstate"
    case "c05050": return "theCircleUndone.theWitchingHour"
    case "c05065": return "theCircleUndone.atDeathsDoorstep"
    case "c05120": return "theCircleUndone.theSecretName"
    case "c05161": return "theCircleUndone.theWagesOfSin"
    case "c05197": return "theCircleUndone.forTheGreaterGood"
    case "c05238": return "theCircleUndone.unionAndDisillusion"
    case "c05284": return "theCircleUndone.inTheClutchesOfChaos"
    case "c05325": return "theCircleUndone.beforeTheBlackThrone"
    case "c06039": return "dreamEaters.beyondTheGatesOfSleep"
    case "c06063": return "dreamEaters.wakingNightmare"
    case "c06119": return "dreamEaters.theSearchForKadath"
    case "c06168": return "dreamEaters.aThousandShapesOfHorror"
    case "c06206": return "dreamEaters.darkSideOfTheMoon"
    case "c06247": return "dreamEaters.pointOfNoReturn"
    case "c06286": return "dreamEaters.whereTheGodsDwell"
    case "c06333": return "dreamEaters.weaverOfTheCosmos"
    default: throw new Error(`Unknown scenario id: ${scenario.id}`)
  }
}
