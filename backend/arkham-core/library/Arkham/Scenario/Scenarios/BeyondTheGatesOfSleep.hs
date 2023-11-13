module Arkham.Scenario.Scenarios.BeyondTheGatesOfSleep (
  BeyondTheGatesOfSleep (..),
  beyondTheGatesOfSleep,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Runner
import Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorText
import Arkham.Trait (
  Trait (Assistant, Criminal, Drifter, Hunter, Medic, Miskatonic, Scholar, Veteran, Wayfarer),
 )
import Data.Aeson (Result (..))

newtype BeyondTheGatesOfSleep = BeyondTheGatesOfSleep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheGatesOfSleep :: Difficulty -> BeyondTheGatesOfSleep
beyondTheGatesOfSleep difficulty =
  scenarioWith
    BeyondTheGatesOfSleep
    "06039"
    "Beyond the Gates of Sleep"
    difficulty
    []
    $ metaL
    .~ toJSON ([] :: [Dream])

data Dream
  = GuardianDream
  | SeekerDream
  | RogueDream
  | MysticDream
  | SurvivorDream
  | CriminalDream
  | DrifterDream
  | HunterDream
  | MedicOrAssistantDream
  | MiskatonicOrScholarDream
  | VeteranDream
  | WayfarerDream
  | NeutralDream1
  | NeutralDream2
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

dreamLabel :: Dream -> Text
dreamLabel = \case
  GuardianDream -> "Guardian Dream"
  SeekerDream -> "Seeker Dream"
  RogueDream -> "Rogue Dream"
  MysticDream -> "Mystic Dream"
  SurvivorDream -> "Survivor Dream"
  CriminalDream -> "Criminal Dream"
  DrifterDream -> "Drifter Dream"
  HunterDream -> "Hunter Dream"
  MedicOrAssistantDream -> "Medic or Assistant Dream"
  MiskatonicOrScholarDream -> "Miskatonic or Scholar Dream"
  VeteranDream -> "Veteran Dream"
  WayfarerDream -> "Wayfarer Dream"
  NeutralDream1 -> "Neutral Dream 1"
  NeutralDream2 -> "Neutral Dream 2"

dreamEffect :: InvestigatorId -> Dream -> [Message]
dreamEffect iid = \case
  MysticDream -> [search iid GameSource iid [fromDeck] (CardWithClass Mystic) (PlayFound iid 1)]
  _ -> []

dreamsMap :: Map Dream FlavorText
dreamsMap =
  mapFromList
    [ (GuardianDream, guardianDream)
    , (SeekerDream, seekerDream)
    , (RogueDream, rogueDream)
    , (MysticDream, mysticDream)
    , (SurvivorDream, survivorDream)
    , (CriminalDream, criminalDream)
    , (DrifterDream, drifterDream)
    , (HunterDream, hunterDream)
    , (MedicOrAssistantDream, medicOrAssistantDream)
    , (MiskatonicOrScholarDream, miskatonicOrScholarDream)
    , (VeteranDream, veteranDream)
    , (WayfarerDream, wayfarerDream)
    , (NeutralDream1, neutralDream1)
    , (NeutralDream2, neutralDream2)
    ]

classDreams :: ClassSymbol -> [Dream]
classDreams Guardian = [GuardianDream, NeutralDream1, NeutralDream2]
classDreams Seeker = [SeekerDream, NeutralDream1, NeutralDream2]
classDreams Rogue = [RogueDream, NeutralDream1, NeutralDream2]
classDreams Mystic = [MysticDream, NeutralDream1, NeutralDream2]
classDreams Survivor = [SurvivorDream, NeutralDream1, NeutralDream2]
classDreams Neutral = [NeutralDream1, NeutralDream2]
classDreams Mythos = []

traitsDreams :: [Trait] -> [Dream]
traitsDreams = nub . concatMap traitDreams
 where
  traitDreams = \case
    Criminal -> [CriminalDream]
    Drifter -> [DrifterDream]
    Hunter -> [HunterDream]
    Medic -> [MedicOrAssistantDream]
    Assistant -> [MedicOrAssistantDream]
    Miskatonic -> [MiskatonicOrScholarDream]
    Scholar -> [MiskatonicOrScholarDream]
    Veteran -> [VeteranDream]
    Wayfarer -> [WayfarerDream]
    _ -> []

instance HasChaosTokenValue BeyondTheGatesOfSleep where
  getChaosTokenValue iid tokenFace (BeyondTheGatesOfSleep attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage BeyondTheGatesOfSleep where
  runMessage msg s@(BeyondTheGatesOfSleep attrs) = case msg of
    ForInvestigator i Setup -> do
      let
        usedDreams = case fromJSON (scenarioMeta attrs) of
          Error e -> error $ "failed to parse drams: " <> e
          Success result -> result
      investigatorClass <- field InvestigatorClass i
      traits <- field InvestigatorTraits i
      player <- getPlayer i
      players <- allPlayers

      let
        allDreams = classDreams investigatorClass <> traitsDreams (toList traits)
        unusedDreams = allDreams \\ usedDreams
        availableDreams = if null unusedDreams then allDreams else unusedDreams
        chooseDream dream =
          Label
            (dreamLabel dream)
            $ [ story players $ findWithDefault (error "missing dream") dream dreamsMap
              , SetScenarioMeta $ toJSON $ dream : usedDreams
              ]
            <> dreamEffect i dream

      push $ chooseOne player $ map chooseDream availableDreams
      pure s
    Setup -> do
      investigators <- allInvestigators
      push $ EndOfGame Nothing
      pushAll $ map (`ForInvestigator` Setup) investigators
      pure s
    _ -> BeyondTheGatesOfSleep <$> runMessage msg attrs
