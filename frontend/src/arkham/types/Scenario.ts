import * as JsonDecoder from 'ts.data.json';
import { type Search, searchDecoder } from '@/arkham/types/Search';
import { type Name, nameDecoder } from '@/arkham/types/Name';
import { CampaignStep, campaignStepDecoder} from '@/arkham/types/CampaignStep';
import { v2Optional } from '@/arkham/parser';
import {
  Card,
  cardDecoder,
  CardContents,
  cardContentsDecoder,
} from '@/arkham/types/Card';
import { ChaosBag, chaosBagDecoder } from '@/arkham/types/ChaosBag';
import { LogContents, logContentsDecoder } from '@/arkham/types/Log';
import { ArkhamKey, arkhamKeyDecoder } from '@/arkham/types/Key';
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
    title: JsonDecoder.string(),
    subtitle: JsonDecoder.nullable(JsonDecoder.string()),
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
  additionalReferences: string[];
  difficulty: Difficulty;
  locationLayout: string[] | null;
  usesGrid: boolean;
  decksLayout: string[];
  decks: [string, Card[]][];
  cardsUnderScenarioReference: Card[];
  cardsUnderAgendaDeck: Card[];
  cardsUnderActDeck: Card[];
  cardsNextToActDeck: Card[];
  foundCards: Record<string, Card[]>;
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
  started: boolean;
  encounterDeck: CardContents[];
  tarotCards: TarotCard[];
  xpBreakdown?: XpEntry[];
  meta: any;
  log: Remembered[];
  storyCards: { [key: string]: CardContents[] };
  campaignStep: CampaignStep | null;
}

export const scenarioDeckDecoder = JsonDecoder.object({
  tag: JsonDecoder.string(),
  contents: JsonDecoder.array<Card>(cardDecoder, 'Card[]').map(cards => cards.length)
}, 'ScenarioDeck').map(({tag, contents}) => ({ tag, deckSize: contents }))


export const scenarioDetailsDecoder = JsonDecoder.object<ScenarioDetails>({
  id: JsonDecoder.string(),
  difficulty: difficultyDecoder,
  name: scenarioNameDecoder,
}, 'ScenarioDetails');

export type Remembered=
    { tag: "YouOweBiancaResources", contents: number } |
    { tag: "RememberNamed", actualTag: string, name: Name } |
    { tag: string }

const rememberedDecoder = JsonDecoder.oneOf([
  JsonDecoder.object<Remembered>(
    {
      tag: JsonDecoder.literal('YouOweBiancaResources'),
      contents: JsonDecoder.tuple(
        [JsonDecoder.succeed(), JsonDecoder.number()],
        'YouOweBiancaResources'
      ).map<number>((res: [any, number]) => res[1])
    },
    'Remembered'
  ),
  JsonDecoder.object(
    {
      tag: JsonDecoder.string(),
      contents: JsonDecoder.object({ getLabel: nameDecoder, unLabel: JsonDecoder.string() }, 'labeled').map(res => res.getLabel)
    },
    'Remembered').map(({tag, contents}) => ({ tag: "RememberedName", actualTag: tag, name: contents })),
  JsonDecoder.object<Remembered>( { tag: JsonDecoder.string() }, 'Remembered')
], 'Remembered');



export const scenarioDecoder = JsonDecoder.object<Scenario>({
  name: scenarioNameDecoder,
  id: JsonDecoder.string(),
  meta: JsonDecoder.succeed(),
  reference: JsonDecoder.string(),
  additionalReferences: JsonDecoder.array(JsonDecoder.string(), 'string[]'),
  log: JsonDecoder.array(rememberedDecoder, 'remembered[]'),
  difficulty: difficultyDecoder,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string(), 'GridLayout[]')),
  usesGrid: JsonDecoder.boolean(),
  decksLayout: JsonDecoder.array<string>(JsonDecoder.string(), 'GridLayout[]'),
  decks: JsonDecoder.array<[string, Card[]]>(JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.array<Card>(cardDecoder, 'Card[]')], '[string, Card[]]'), '[string, Card[]][]'),
  cardsUnderScenarioReference: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathAgendaCards'),
  cardsUnderActDeck: JsonDecoder.array<Card>(cardDecoder, 'UnderneathActCards'),
  tarotCards: JsonDecoder.
    array(
      JsonDecoder.tuple([tarotScopeDecoder, JsonDecoder.array(tarotCardDecoder, 'TarotCard[]')], '[TarotScope, TarotCard[]]'),
      '[TarotScope, TarotCard[]][]'
    ).map(res => res.reduce<TarotCard[]>((acc, [k, vs]) => [...acc, ...vs.map(v => ({ ...v, scope: k }))], [])),
  search: v2Optional(searchDecoder).map((search: Search) => search?.searchFoundCards || {}),
  counts:
    JsonDecoder.array<[string, number]>(
      JsonDecoder.oneOf([
        JsonDecoder.tuple([JsonDecoder.string(), JsonDecoder.number()], '[string, number]'),
        JsonDecoder.tuple([JsonDecoder.object(
            { tag: JsonDecoder.string() }
            , 'count with arg'
        ), JsonDecoder.number()], '[string, number]').map(([a, b]) => [a.tag, b]),
      ], 'counts'),
      '[string, number][]'
    ).
    map<Record<string, number>>(res => {
      return res.reduce<Record<string, number>>((acc, [k, v]) => {
        acc[k] = v
        return acc
      }, {})
    }),
  cardsNextToActDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToActDeck'),
  cardsNextToAgendaDeck: JsonDecoder.array<Card>(cardDecoder, 'CardsNextToAgendaDeck'),
  setAsideKeys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  keys: JsonDecoder.array<ArkhamKey>(arkhamKeyDecoder, 'Key[]'),
  setAsideCards: JsonDecoder.array<Card>(cardDecoder, 'SetAsideCards'),
  chaosBag: chaosBagDecoder,
  discard: JsonDecoder.array<CardContents>(cardContentsDecoder, 'EncounterCardContents[]'),
  victoryDisplay: JsonDecoder.array<Card>(cardDecoder, 'Card[]'),
  standaloneCampaignLog: logContentsDecoder,
  tokens: tokensDecoder,

  encounterDeck: JsonDecoder.array<CardContents>(cardContentsDecoder, 'CardContents[]'),
  xpBreakdown: v2Optional(JsonDecoder.array<XpEntry>(xpEntryDecoder, 'XpEntry[]')),
  hasEncounterDeck: JsonDecoder.boolean(),
  started: JsonDecoder.boolean(),
  encounterDecks: JsonDecoder.array<[string, [CardContents[], CardContents[]]]>(
    JsonDecoder.tuple([
      JsonDecoder.string(),
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
  storyCards: JsonDecoder.record(JsonDecoder.array(cardDecoder, 'CardDef[]'), 'CardDef[]'),
  campaignStep: JsonDecoder.nullable(campaignStepDecoder),
}, 'Scenario').map(({search, ...rest}) => ({...rest, foundCards: search}));

export function scenarioToKeyI18n(scenario: Scenario): string {
  const full = scenarioToI18n(scenario);
  return full.split('.')[0] || "base"
}

export function scenarioToI18n(scenario: Scenario): string {
  return scenarioIdToI18n(scenario.id);
}

export function scenarioIdToI18n(scenarioId: string): string {
  switch (scenarioId.replace(/^c/, '')) {
    case "01104": return "nightOfTheZealot.theGathering"
    case "01120": return "nightOfTheZealot.theMidnightMasks"
    case "01142": return "nightOfTheZealot.theDevourerBelow"
    case "02041": return "theDunwichLegacy.extracurricularActivity"
    case "02062": return "theDunwichLegacy.theHouseAlwaysWins"
    case "02118": return "theDunwichLegacy.theMiskatonicMuseum"
    case "02159": return "theDunwichLegacy.theEssexCountyExpress"
    case "02195": return "theDunwichLegacy.bloodOnTheAltar"
    case "02236": return "theDunwichLegacy.undimensionedAndUnseen"
    case "02274": return "theDunwichLegacy.whereDoomAwaits"
    case "02311": return "theDunwichLegacy.lostInTimeAndSpace"
    case "03043": return "thePathToCarcosa.curtainCall"
    case "03061": return "thePathToCarcosa.theLastKing"
    case "03120": return "thePathToCarcosa.echoesOfThePast"
    case "03159": return "thePathToCarcosa.theUnspeakableOath"
    case "03200": return "thePathToCarcosa.aPhantomOfTruth"
    case "03240": return "thePathToCarcosa.thePallidMask"
    case "03274": return "thePathToCarcosa.blackStarsRise"
    case "03316": return "thePathToCarcosa.dimCarcosa"
    case "04043": return "theForgottenAge.theUntamedWilds"
    case "04054": return "theForgottenAge.theDoomOfEztli"
    case "04113": return "theForgottenAge.threadsOfFate"
    case "04161": return "theForgottenAge.theBoundaryBeyond"
    case "04205": return "theForgottenAge.heartOfTheElders"
    case "04205a": return "theForgottenAge.heartOfTheEldersPart1"
    case "04205b": return "theForgottenAge.heartOfTheEldersPart2"
    case "04237": return "theForgottenAge.theCityOfArchives"
    case "04277": return "theForgottenAge.theDepthsOfYoth"
    case "04314": return "theForgottenAge.shatteredAeons"
    case "04344": return "theForgottenAge.turnBackTime"
    case "05043": return "theCircleUndone.disappearanceAtTheTwilightEstate"
    case "05050": return "theCircleUndone.theWitchingHour"
    case "05065": return "theCircleUndone.atDeathsDoorstep"
    case "05120": return "theCircleUndone.theSecretName"
    case "05161": return "theCircleUndone.theWagesOfSin"
    case "05197": return "theCircleUndone.forTheGreaterGood"
    case "05238": return "theCircleUndone.unionAndDisillusion"
    case "05284": return "theCircleUndone.inTheClutchesOfChaos"
    case "05325": return "theCircleUndone.beforeTheBlackThrone"
    case "06039": return "theDreamEaters.beyondTheGatesOfSleep"
    case "06063": return "theDreamEaters.wakingNightmare"
    case "06119": return "theDreamEaters.theSearchForKadath"
    case "06168": return "theDreamEaters.aThousandShapesOfHorror"
    case "06206": return "theDreamEaters.darkSideOfTheMoon"
    case "06247": return "theDreamEaters.pointOfNoReturn"
    case "06286": return "theDreamEaters.whereTheGodsDwell"
    case "06333": return "theDreamEaters.weaverOfTheCosmos"
    case "07041": return "theInnsmouthConspiracy.thePitOfDespair"
    case "07056": return "theInnsmouthConspiracy.theVanishingOfElinaHarper"
    case "07123": return "theInnsmouthConspiracy.inTooDeep"
    case "07163": return "theInnsmouthConspiracy.devilReef"
    case "07198": return "theInnsmouthConspiracy.horrorInHighGear"
    case "07231": return "theInnsmouthConspiracy.aLightInTheFog"
    case "07274": return "theInnsmouthConspiracy.theLairOfDagon"
    case "07311": return "theInnsmouthConspiracy.intoTheMaelstrom"
    case "08501a": return "edgeOfTheEarth.iceAndDeath"
    case "08501b": return "edgeOfTheEarth.iceAndDeath"
    case "08501c": return "edgeOfTheEarth.iceAndDeath"
    case "08549": return "edgeOfTheEarth.fatalMirage"
    case "08596": return "edgeOfTheEarth.toTheForbiddenPeaks"
    case "08621": return "edgeOfTheEarth.cityOfTheElderThings"
    case "08648a": return "edgeOfTheEarth.theHeartOfMadness"
    case "08648b": return "edgeOfTheEarth.theHeartOfMadness"
    case "09501": return "theScarletKeys.riddlesAndRain"
    case "09520": return "theScarletKeys.deadHeat"
    case "09545": return "theScarletKeys.sanguineShadows"
    case "09566": return "theScarletKeys.dealingsInTheDark"
    case "09591": return "theScarletKeys.dancingMad"
    case "09609": return "theScarletKeys.onThinIce"
    case "09635": return "theScarletKeys.dogsOfWar"
    case "09660": return "theScarletKeys.shadesOfSuffering"
    case "50011": return "nightOfTheZealot.theGathering"
    case "50025": return "nightOfTheZealot.theMidnightMasks"
    case "50032": return "nightOfTheZealot.theDevourerBelow"
    case "51012": return "theDunwichLegacy.extracurricularActivity"
    case "51015": return "theDunwichLegacy.theHouseAlwaysWins"
    case "51020": return "theDunwichLegacy.theMiskatonicMuseum"
    case "51025": return "theDunwichLegacy.theEssexCountyExpress"
    case "51032": return "theDunwichLegacy.bloodOnTheAltar"
    case "51041": return "theDunwichLegacy.undimensionedAndUnseen"
    case "51047": return "theDunwichLegacy.whereDoomAwaits"
    case "51053": return "theDunwichLegacy.lostInTimeAndSpace"
    case "52014": return "thePathToCarcosa.curtainCall"
    case "52021": return "thePathToCarcosa.theLastKing"
    case "52028": return "thePathToCarcosa.echoesOfThePast"
    case "52034": return "thePathToCarcosa.theUnspeakableOath"
    case "52040": return "thePathToCarcosa.aPhantomOfTruth"
    case "52048": return "thePathToCarcosa.thePallidMask"
    case "52054": return "thePathToCarcosa.blackStarsRise"
    case "52059": return "thePathToCarcosa.dimCarcosa"
    case "53016": return "theForgottenAge.theUntamedWilds"
    case "53017": return "theForgottenAge.theDoomOfEztli"
    case "53028": return "theForgottenAge.threadsOfFate"
    case "53038": return "theForgottenAge.theBoundaryBeyond"
    case "53045": return "theForgottenAge.heartOfTheEldersPart1"
    case "53048": return "theForgottenAge.heartOfTheEldersPart2"
    case "53053": return "theForgottenAge.theCityOfArchives"
    case "53059": return "theForgottenAge.theDepthsOfYoth"
    case "53061": return "theForgottenAge.shatteredAeons"
    case "53066": return "theForgottenAge.turnBackTime"
    case "54016": return "theCircleUndone.disappearanceAtTheTwilightEstate"
    case "54017": return "theCircleUndone.theWitchingHour"
    case "54024": return "theCircleUndone.atDeathsDoorstep"
    case "54029": return "theCircleUndone.theSecretName"
    case "54034": return "theCircleUndone.theWagesOfSin"
    case "54042": return "theCircleUndone.forTheGreaterGood"
    case "54046": return "theCircleUndone.unionAndDisillusion"
    case "54049": return "theCircleUndone.inTheClutchesOfChaos"
    case "54056": return "theCircleUndone.beforeTheBlackThrone"
    case "71001": return "standalone.theMidwinterGala"
    case "72001": return "standalone.filmFatale"
    case "81001": return "standalone.curseOfTheRougarou"
    case "82001": return "standalone.carnevaleOfHorrors"
    case "84001": return "standalone.murderAtTheExcelsiorHotel"
    case "88001": return "standalone.fortuneAndFolly"
    case "88001b": return "standalone.fortuneAndFolly"
    default: throw new Error(`Unknown scenario id: ${scenarioId}`)
  }
}
