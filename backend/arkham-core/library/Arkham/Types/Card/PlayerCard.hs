module Arkham.Types.Card.PlayerCard where

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Card.PlayerCard.Cards
import Arkham.Types.Card.PlayerCard.Type
import Arkham.Types.ClassSymbol
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

data PlayerCard
  = PlaceholderAsset' PlaceholderAsset
  | Rolands38Special' Rolands38Special
  | CoverUp' CoverUp
  | DaisysToteBag' DaisysToteBag
  | TheNecronomicon' TheNecronomicon
  | OnTheLam' OnTheLam
  | HospitalDebts' HospitalDebts
  | HeirloomOfHyperborea' HeirloomOfHyperborea
  | DarkMemory' DarkMemory
  | WendysAmulet' WendysAmulet
  | AbandonedAndAlone' AbandonedAndAlone
  | FortyFiveAutomatic' FortyFiveAutomatic
  | PhysicalTraining' PhysicalTraining
  | BeatCop' BeatCop
  | FirstAid' FirstAid
  | Machete' Machete
  | GuardDog' GuardDog
  | Evidence' Evidence
  | Dodge' Dodge
  | DynamiteBlast' DynamiteBlast
  | ViciousBlow' ViciousBlow
  | ExtraAmmunition1' ExtraAmmunition1
  | PoliceBadge2' PoliceBadge2
  | BeatCop2' BeatCop2
  | Shotgun4' Shotgun4
  | MagnifyingGlass' MagnifyingGlass
  | OldBookOfLore' OldBookOfLore
  | ResearchLibrarian' ResearchLibrarian
  | DrMilanChristopher' DrMilanChristopher
  | Hyperawareness' Hyperawareness
  | MedicalTexts' MedicalTexts
  | MindOverMatter' MindOverMatter
  | WorkingAHunch' WorkingAHunch
  | Barricade' Barricade
  | Deduction' Deduction
  | MagnifyingGlass1' MagnifyingGlass1
  | DiscOfItzamna2' DiscOfItzamna2
  | Encyclopedia2' Encyclopedia2
  | CrypticResearch4' CrypticResearch4
  | Switchblade' Switchblade
  | Burglary' Burglary
  | Pickpocketing' Pickpocketing
  | FortyOneDerringer' FortyOneDerringer
  | LeoDeLuca' LeoDeLuca
  | HardKnocks' HardKnocks
  | Elusive' Elusive
  | Backstab' Backstab
  | SneakAttack' SneakAttack
  | Opportunist' Opportunist
  | LeoDeLuca1' LeoDeLuca1
  | CatBurgler1' CatBurgler1
  | SureGamble3' SureGamble3
  | HotStreak4' HotStreak4
  | ForbiddenKnowledge' ForbiddenKnowledge
  | HolyRosary' HolyRosary
  | Shrivelling' Shrivelling
  | Scrying' Scrying
  | ArcaneStudies' ArcaneStudies
  | ArcaneInitiate' ArcaneInitiate
  | DrawnToTheFlame' DrawnToTheFlame
  | WardOfProtection' WardOfProtection
  | BlindingLight' BlindingLight
  | Fearless' Fearless
  | MindWipe1' MindWipe1
  | BlindingLight2' BlindingLight2
  | BookOfShadows3' BookOfShadows3
  | GrotesqueStatue4' GrotesqueStatue4
  | LeatherCoat' LeatherCoat
  | Scavenging' Scavenging
  | BaseballBat' BaseballBat
  | RabbitsFoot' RabbitsFoot
  | StrayCat' StrayCat
  | DigDeep' DigDeep
  | CunningDistraction' CunningDistraction
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | SurvivalInstinct' SurvivalInstinct
  | Aquinnah1' Aquinnah1
  | CloseCall2' CloseCall2
  | Knife' Knife
  | Flashlight' Flashlight
  | EmergencyCache' EmergencyCache
  | Guts' Guts
  | Perception' Perception
  | Overpower' Overpower
  | ManualDexterity' ManualDexterity
  | UnexpectedCourage' UnexpectedCourage
  | BulletproofVest3' BulletproofVest3
  | ElderSignAmulet3' ElderSignAmulet3
  | Amnesia' Amnesia
  | Paranoia' Paranoia
  | Haunted' Haunted
  | Psychosis' Psychosis
  | Hypochondria' Hypochondria
  | MobEnforcer' MobEnforcer
  | SilverTwilightAcolyte' SilverTwilightAcolyte
  | StubbornDetective' StubbornDetective
  | LitaChantler' LitaChantler
  | ZoeysCross' ZoeysCross
  | SmiteTheWicked' SmiteTheWicked
  | SearchForTheTruth' SearchForTheTruth
  | RexsCurse' RexsCurse
  | JennysTwin45s' JennysTwin45s
  | SearchingForIzzie' SearchingForIzzie
  | JimsTrumpet' JimsTrumpet
  | FinalRhapsody' FinalRhapsody
  | Duke' Duke
  | WrackedByNightmares' WrackedByNightmares
  | Bandolier' Bandolier
  | PhysicalTraining2' PhysicalTraining2
  | DynamiteBlast2' DynamiteBlast2
  | Hyperawareness2' Hyperawareness2
  | Barricade3' Barricade3
  | HardKnocks2' HardKnocks2
  | HotStreak2' HotStreak2
  | ArcaneStudies2' ArcaneStudies2
  | MindWipe3' MindWipe3
  | DigDeep2' DigDeep2
  | RabbitsFoot3' RabbitsFoot3
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasCardCode PlayerCard where
  getCardCode = pcCardCode . playerCardAttrs

instance HasCardId PlayerCard where
  getCardId = pcId . playerCardAttrs

instance Eq PlayerCard where
  a == b = getCardId a == getCardId b

instance HasCost PlayerCard where
  getCost c = case pcCost (playerCardAttrs c) of
    StaticCost n -> n
    DynamicCost -> 0

lookupPlayerCard :: CardCode -> (CardId -> PlayerCard)
lookupPlayerCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ HashMap.lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode (CardId -> PlayerCard)
allPlayerCards = HashMap.fromList
  [ ("asset", PlaceholderAsset' . placeholderAsset)
  , ("01006", Rolands38Special' . rolands38Special)
  , ("01007", CoverUp' . coverUp)
  , ("01008", DaisysToteBag' . daisysToteBag)
  , ("01009", TheNecronomicon' . theNecronomicon)
  , ("01010", OnTheLam' . onTheLam)
  , ("01011", HospitalDebts' . hospitalDebts)
  , ("01012", HeirloomOfHyperborea' . heirloomOfHyperborea)
  , ("01013", DarkMemory' . darkMemory)
  , ("01014", WendysAmulet' . wendysAmulet)
  , ("01015", AbandonedAndAlone' . abandonedAndAlone)
  , ("01016", FortyFiveAutomatic' . fortyFiveAutomatic)
  , ("01017", PhysicalTraining' . physicalTraining)
  , ("01018", BeatCop' . beatCop)
  , ("01019", FirstAid' . firstAid)
  , ("01020", Machete' . machete)
  , ("01021", GuardDog' . guardDog)
  , ("01022", Evidence' . evidence)
  , ("01023", Dodge' . dodge)
  , ("01024", DynamiteBlast' . dynamiteBlast)
  , ("01025", ViciousBlow' . viciousBlow)
  , ("01026", ExtraAmmunition1' . extraAmmunition1)
  , ("01027", PoliceBadge2' . policeBadge2)
  , ("01028", BeatCop2' . beatCop2)
  , ("01029", Shotgun4' . shotgun4)
  , ("01030", MagnifyingGlass' . magnifyingGlass)
  , ("01031", OldBookOfLore' . oldBookOfLore)
  , ("01032", ResearchLibrarian' . researchLibrarian)
  , ("01033", DrMilanChristopher' . drMilanChristopher)
  , ("01034", Hyperawareness' . hyperawareness)
  , ("01035", MedicalTexts' . medicalTexts)
  , ("01036", MindOverMatter' . mindOverMatter)
  , ("01037", WorkingAHunch' . workingAHunch)
  , ("01038", Barricade' . barricade)
  , ("01039", Deduction' . deduction)
  , ("01040", MagnifyingGlass1' . magnifyingGlass1)
  , ("01041", DiscOfItzamna2' . discOfItzamna2)
  , ("01042", Encyclopedia2' . encyclopedia2)
  , ("01043", CrypticResearch4' . crypticResearch4)
  , ("01044", Switchblade' . switchblade)
  , ("01045", Burglary' . burglary)
  , ("01046", Pickpocketing' . pickpocketing)
  , ("01047", FortyOneDerringer' . fortyOneDerringer)
  , ("01048", LeoDeLuca' . leoDeLuca)
  , ("01049", HardKnocks' . hardKnocks)
  , ("01050", Elusive' . elusive)
  , ("01051", Backstab' . backstab)
  , ("01052", SneakAttack' . sneakAttack)
  , ("01053", Opportunist' . opportunist)
  , ("01054", LeoDeLuca1' . leoDeLuca1)
  , ("01055", CatBurgler1' . catBurgler1)
  , ("01056", SureGamble3' . sureGamble3)
  , ("01057", HotStreak4' . hotStreak4)
  , ("01058", ForbiddenKnowledge' . forbiddenKnowledge)
  , ("01059", HolyRosary' . holyRosary)
  , ("01060", Shrivelling' . shrivelling)
  , ("01061", Scrying' . scrying)
  , ("01062", ArcaneStudies' . arcaneStudies)
  , ("01063", ArcaneInitiate' . arcaneInitiate)
  , ("01064", DrawnToTheFlame' . drawnToTheFlame)
  , ("01065", WardOfProtection' . wardOfProtection)
  , ("01066", BlindingLight' . blindingLight)
  , ("01067", Fearless' . fearless)
  , ("01068", MindWipe1' . mindWipe1)
  , ("01069", BlindingLight2' . blindingLight2)
  , ("01070", BookOfShadows3' . bookOfShadows3)
  , ("01071", GrotesqueStatue4' . grotesqueStatue4)
  , ("01072", LeatherCoat' . leatherCoat)
  , ("01073", Scavenging' . scavenging)
  , ("01074", BaseballBat' . baseballBat)
  , ("01075", RabbitsFoot' . rabbitsFoot)
  , ("01076", StrayCat' . strayCat)
  , ("01077", DigDeep' . digDeep)
  , ("01078", CunningDistraction' . cunningDistraction)
  , ("01079", LookWhatIFound' . lookWhatIFound)
  , ("01080", Lucky' . lucky)
  , ("01081", SurvivalInstinct' . survivalInstinct)
  , ("01082", Aquinnah1' . aquinnah1)
  , ("01083", CloseCall2' . closeCall2)
  , ("01086", Knife' . knife)
  , ("01087", Flashlight' . flashlight)
  , ("01088", EmergencyCache' . emergencyCache)
  , ("01089", Guts' . guts)
  , ("01090", Perception' . perception)
  , ("01091", Overpower' . overpower)
  , ("01092", ManualDexterity' . manualDexterity)
  , ("01093", UnexpectedCourage' . unexpectedCourage)
  , ("01094", BulletproofVest3' . bulletproofVest3)
  , ("01095", ElderSignAmulet3' . elderSignAmulet3)
  , ("01096", Amnesia' . amnesia)
  , ("01097", Paranoia' . paranoia)
  , ("01098", Haunted' . haunted)
  , ("01099", Psychosis' . psychosis)
  , ("01100", Hypochondria' . hypochondria)
  , ("01101", MobEnforcer' . mobEnforcer)
  , ("01102", SilverTwilightAcolyte' . silverTwilightAcolyte)
  , ("01103", StubbornDetective' . stubbornDetective)
  , ("01117", LitaChantler' . litaChantler)
  , ("02006", ZoeysCross' . zoeysCross)
  , ("02007", SmiteTheWicked' . smiteTheWicked)
  , ("02008", SearchForTheTruth' . searchForTheTruth)
  , ("02009", RexsCurse' . rexsCurse)
  , ("02010", JennysTwin45s' . jennysTwin45s)
  , ("02011", SearchingForIzzie' . searchingForIzzie)
  , ("02012", JimsTrumpet' . jimsTrumpet)
  , ("02013", FinalRhapsody' . finalRhapsody)
  , ("02014", Duke' . duke)
  , ("02015", WrackedByNightmares' . wrackedByNightmares)
  , ("02147", Bandolier' . bandolier)
  , ("50001", PhysicalTraining2' . physicalTraining2)
  , ("50002", DynamiteBlast2' . dynamiteBlast2)
  , ("50003", Hyperawareness2' . hyperawareness2)
  , ("50004", Barricade3' . barricade3)
  , ("50005", HardKnocks2' . hardKnocks2)
  , ("50006", HotStreak2' . hotStreak2)
  , ("50007", ArcaneStudies2' . arcaneStudies2)
  , ("50008", MindWipe3' . mindWipe3)
  , ("50009", DigDeep2' . digDeep2)
  , ("50010", RabbitsFoot3' . rabbitsFoot3)
  ]

newtype PlaceholderAsset = PlaceholderAsset Attrs
  deriving newtype (Show, ToJSON, FromJSON)

basePlayerCard
  :: CardId
  -> CardCode
  -> Text
  -> Int
  -> PlayerCardType
  -> ClassSymbol
  -> PlayerCard
basePlayerCard cardId cardCode name cost cardType classSymbol =
  PlaceholderAsset' . PlaceholderAsset $ Attrs
    { pcCardCode = cardCode
    , pcName = name
    , pcCost = StaticCost cost
    , pcLevel = 0
    , pcCardType = cardType
    , pcWeakness = False
    , pcBearer = Nothing
    , pcClassSymbol = classSymbol
    , pcSkills = mempty
    , pcTraits = mempty
    , pcKeywords = mempty
    , pcFast = False
    , pcWindows = mempty
    , pcId = cardId
    , pcAction = Nothing
    , pcRevelation = False
    , pcVictoryPoints = Nothing
    , pcCommitRestrictions = mempty
    , pcAttackOfOpportunityModifiers = mempty
    }


placeholderAsset :: CardId -> PlaceholderAsset
placeholderAsset cardId =
  PlaceholderAsset $ asset cardId "asset" "Placeholder Asset" 0 Neutral

playerCardAttrs :: PlayerCard -> Attrs
playerCardAttrs = \case
  PlaceholderAsset' attrs -> coerce attrs
  Rolands38Special' attrs -> coerce attrs
  CoverUp' attrs -> coerce attrs
  DaisysToteBag' attrs -> coerce attrs
  TheNecronomicon' attrs -> coerce attrs
  OnTheLam' attrs -> coerce attrs
  HospitalDebts' attrs -> coerce attrs
  HeirloomOfHyperborea' attrs -> coerce attrs
  DarkMemory' attrs -> coerce attrs
  WendysAmulet' attrs -> coerce attrs
  AbandonedAndAlone' attrs -> coerce attrs
  FortyFiveAutomatic' attrs -> coerce attrs
  PhysicalTraining' attrs -> coerce attrs
  BeatCop' attrs -> coerce attrs
  FirstAid' attrs -> coerce attrs
  Machete' attrs -> coerce attrs
  GuardDog' attrs -> coerce attrs
  Evidence' attrs -> coerce attrs
  Dodge' attrs -> coerce attrs
  DynamiteBlast' attrs -> coerce attrs
  ViciousBlow' attrs -> coerce attrs
  ExtraAmmunition1' attrs -> coerce attrs
  PoliceBadge2' attrs -> coerce attrs
  BeatCop2' attrs -> coerce attrs
  Shotgun4' attrs -> coerce attrs
  MagnifyingGlass' attrs -> coerce attrs
  OldBookOfLore' attrs -> coerce attrs
  ResearchLibrarian' attrs -> coerce attrs
  DrMilanChristopher' attrs -> coerce attrs
  Hyperawareness' attrs -> coerce attrs
  MedicalTexts' attrs -> coerce attrs
  MindOverMatter' attrs -> coerce attrs
  WorkingAHunch' attrs -> coerce attrs
  Barricade' attrs -> coerce attrs
  Deduction' attrs -> coerce attrs
  MagnifyingGlass1' attrs -> coerce attrs
  DiscOfItzamna2' attrs -> coerce attrs
  Encyclopedia2' attrs -> coerce attrs
  CrypticResearch4' attrs -> coerce attrs
  Switchblade' attrs -> coerce attrs
  Burglary' attrs -> coerce attrs
  Pickpocketing' attrs -> coerce attrs
  FortyOneDerringer' attrs -> coerce attrs
  LeoDeLuca' attrs -> coerce attrs
  HardKnocks' attrs -> coerce attrs
  Elusive' attrs -> coerce attrs
  Backstab' attrs -> coerce attrs
  SneakAttack' attrs -> coerce attrs
  Opportunist' attrs -> coerce attrs
  LeoDeLuca1' attrs -> coerce attrs
  CatBurgler1' attrs -> coerce attrs
  SureGamble3' attrs -> coerce attrs
  HotStreak4' attrs -> coerce attrs
  ForbiddenKnowledge' attrs -> coerce attrs
  HolyRosary' attrs -> coerce attrs
  Shrivelling' attrs -> coerce attrs
  Scrying' attrs -> coerce attrs
  ArcaneStudies' attrs -> coerce attrs
  ArcaneInitiate' attrs -> coerce attrs
  DrawnToTheFlame' attrs -> coerce attrs
  WardOfProtection' attrs -> coerce attrs
  BlindingLight' attrs -> coerce attrs
  Fearless' attrs -> coerce attrs
  MindWipe1' attrs -> coerce attrs
  BlindingLight2' attrs -> coerce attrs
  BookOfShadows3' attrs -> coerce attrs
  GrotesqueStatue4' attrs -> coerce attrs
  LeatherCoat' attrs -> coerce attrs
  Scavenging' attrs -> coerce attrs
  BaseballBat' attrs -> coerce attrs
  RabbitsFoot' attrs -> coerce attrs
  StrayCat' attrs -> coerce attrs
  DigDeep' attrs -> coerce attrs
  CunningDistraction' attrs -> coerce attrs
  LookWhatIFound' attrs -> coerce attrs
  Lucky' attrs -> coerce attrs
  SurvivalInstinct' attrs -> coerce attrs
  Aquinnah1' attrs -> coerce attrs
  CloseCall2' attrs -> coerce attrs
  Knife' attrs -> coerce attrs
  Flashlight' attrs -> coerce attrs
  EmergencyCache' attrs -> coerce attrs
  Guts' attrs -> coerce attrs
  Perception' attrs -> coerce attrs
  Overpower' attrs -> coerce attrs
  ManualDexterity' attrs -> coerce attrs
  UnexpectedCourage' attrs -> coerce attrs
  BulletproofVest3' attrs -> coerce attrs
  ElderSignAmulet3' attrs -> coerce attrs
  Amnesia' attrs -> coerce attrs
  Paranoia' attrs -> coerce attrs
  Haunted' attrs -> coerce attrs
  Psychosis' attrs -> coerce attrs
  Hypochondria' attrs -> coerce attrs
  MobEnforcer' attrs -> coerce attrs
  SilverTwilightAcolyte' attrs -> coerce attrs
  StubbornDetective' attrs -> coerce attrs
  LitaChantler' attrs -> coerce attrs
  ZoeysCross' attrs -> coerce attrs
  SmiteTheWicked' attrs -> coerce attrs
  SearchForTheTruth' attrs -> coerce attrs
  RexsCurse' attrs -> coerce attrs
  JennysTwin45s' attrs -> coerce attrs
  SearchingForIzzie' attrs -> coerce attrs
  JimsTrumpet' attrs -> coerce attrs
  FinalRhapsody' attrs -> coerce attrs
  Duke' attrs -> coerce attrs
  WrackedByNightmares' attrs -> coerce attrs
  Bandolier' attrs -> coerce attrs
  PhysicalTraining2' attrs -> coerce attrs
  DynamiteBlast2' attrs -> coerce attrs
  Hyperawareness2' attrs -> coerce attrs
  Barricade3' attrs -> coerce attrs
  HardKnocks2' attrs -> coerce attrs
  HotStreak2' attrs -> coerce attrs
  ArcaneStudies2' attrs -> coerce attrs
  MindWipe3' attrs -> coerce attrs
  DigDeep2' attrs -> coerce attrs
  RabbitsFoot3' attrs -> coerce attrs
