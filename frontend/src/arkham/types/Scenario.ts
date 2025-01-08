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
import { XpEntry, xpEntryDecoder} from '@/arkham/types/Xp';

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
  keys: ArkhamKey[];
  chaosBag: ChaosBag;
  discard: CardContents[];
  victoryDisplay: Card[];
  standaloneCampaignLog: LogContents | null;
  tokens: Tokens;
  counts: Record<string, number>; // eslint-disable-line
  encounterDecks: Record<string, [CardContents[], CardContents[]]>;
  hasEncounterDeck: boolean;
  tarotCards: TarotCard[];
  xpBreakdown?: XpEntry[];
  meta: any;
  log: Remembered[];
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

export type Remembered = { tag: "YouOweBiancaResources", contents: number } | { tag: string }

const rememberedDecoder = JsonDecoder.oneOf([
  JsonDecoder.object<Remembered>(
    {
      tag: JsonDecoder.isExactly('YouOweBiancaResources'),
      contents: JsonDecoder.tuple<[any, number]>(
        [JsonDecoder.succeed, JsonDecoder.number],
        'YouOweBiancaResources'
      ).map<number>((res: [any, number]) => res[1])
    },
    'Remembered'
  ),
  JsonDecoder.object<Remembered>( { tag: JsonDecoder.string }, 'Remembered')
], 'Remembered');



export const scenarioDecoder = JsonDecoder.object<Scenario>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string,
  meta: JsonDecoder.succeed,
  reference: JsonDecoder.string,
  log: JsonDecoder.array(rememberedDecoder, 'remembered[]'),
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
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
  chaosBag: chaosBagDecoder,
  discard: JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]'),
  victoryDisplay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  xpBreakdown: JsonDecoder.optional(JsonDecoder.array<XpEntry>(xpEntryDecoder, 'XpEntry[]')),
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
    case "c06039": return "theDreamEaters.beyondTheGatesOfSleep"
    case "c06063": return "theDreamEaters.wakingNightmare"
    case "c06119": return "theDreamEaters.theSearchForKadath"
    case "c06168": return "theDreamEaters.aThousandShapesOfHorror"
    case "c06206": return "theDreamEaters.darkSideOfTheMoon"
    case "c06247": return "theDreamEaters.pointOfNoReturn"
    case "c06286": return "theDreamEaters.whereTheGodsDwell"
    case "c06333": return "theDreamEaters.weaverOfTheCosmos"
    case "c07041": return "theInnsmouthConspiracy.thePitOfDespair"
    case "c07056": return "theInnsmouthConspiracy.theVanishingOfElinaHarper"
    case "c07123": return "theInnsmouthConspiracy.inTooDeep"
    case "c07163": return "theInnsmouthConspiracy.devilReef"
    case "c07198": return "theInnsmouthConspiracy.horrorInHighGear"
    case "c07231": return "theInnsmouthConspiracy.aLightInTheFog"
    case "c07274": return "theInnsmouthConspiracy.theLairOfDagon"
    case "c07311": return "theInnsmouthConspiracy.intoTheMaelstrom"
    case "c08501a": return "edgeOfTheEarth.iceAndDeath"
    case "c08501b": return "edgeOfTheEarth.iceAndDeath"
    case "c08501c": return "edgeOfTheEarth.iceAndDeath"
    case "c08549": return "edgeOfTheEarth.fatalMirage"
    case "c08596": return "edgeOfTheEarth.toTheForbiddenPeaks"
    case "c50011": return "nightOfTheZealot.theGathering"
    case "c50025": return "nightOfTheZealot.theMidnightMasks"
    case "c50032": return "nightOfTheZealot.theDevourerBelow"
    case "c81001": return "standalone.curseOfTheRougarou"
    case "c82001": return "standalone.carnevaleOfHorrors"
    case "c84001": return "standalone.murderAtTheExcelsiorHotel"
    default: throw new Error(`Unknown scenario id: ${scenario.id}`)
  }
}
