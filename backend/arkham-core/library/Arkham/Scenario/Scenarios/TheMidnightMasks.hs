module Arkham.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLogKey
import Arkham.Campaigns.NightOfTheZealot.ChaosBag
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher (
  CardMatcher (..),
  EnemyMatcher (..),
  ExtendedCardMatcher (..),
 )
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (forceAddCampaignCardToDeckChoice, recordSetInsert)
import Arkham.Scenario.Runner hiding (createEnemyAt, placeLocationCard, story)
import Arkham.Scenario.Setup
import Arkham.Scenarios.TheMidnightMasks.Story
import Arkham.Token
import Arkham.Trait qualified as Trait
import Data.List qualified as List

newtype TheMidnightMasks = TheMidnightMasks ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  scenarioWith
    TheMidnightMasks
    "01120"
    "The Midnight Masks"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside yourHouse"
    ]
    (decksL .~ mapFromList [(CultistDeck, [])])

instance HasChaosTokenValue TheMidnightMasks where
  getChaosTokenValue iid chaosTokenFace (TheMidnightMasks attrs) = case chaosTokenFace of
    Skull -> do
      value <- byDifficulty attrs (fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)) getDoomCount
      pure $ ChaosTokenValue Skull (NegativeModifier value)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

allCultists :: [CardCode]
allCultists =
  map
    toCardCode
    [ Enemies.wolfManDrew
    , Enemies.hermanCollins
    , Enemies.peterWarren
    , Enemies.victoriaDevereux
    , Enemies.ruthTurner
    , Enemies.theMaskedHunter
    ]

instance RunMessage TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = runQueueT $ case msg of
    SetChaosTokensForScenario -> do
      pushWhenM getIsStandalone $ SetChaosTokens (chaosBagContents $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      litaForcedToFindOthersToHelpHerCause <- getHasRecord LitaWasForcedToFindOthersToHelpHerCause
      story
        $ introPart1
        $ if litaForcedToFindOthersToHelpHerCause then TheMidnightMasksIntroOne else TheMidnightMasksIntroTwo
      story introPart2
      pure s
    Setup -> runScenarioSetup TheMidnightMasks attrs do
      gather EncounterSet.TheMidnightMasks
      gather EncounterSet.ChillingCold
      gather EncounterSet.Nightgaunts
      gather EncounterSet.LockedDoors
      gather EncounterSet.DarkCult

      setAgendaDeck [Agendas.predatorOrPrey, Agendas.timeIsRunningShort]
      setActDeck [Acts.uncoveringTheConspiracy]

      rivertown <- place Locations.rivertown
      southside <- placeOneOf (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
      downtown <- placeOneOf (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
      graveyard <- place Locations.graveyard
      placeAll
        [Locations.easttown, Locations.miskatonicUniversity, Locations.northside, Locations.stMarysHospital]

      houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
      addExtraDeck CultistDeck =<< gatherEncounterSet EncounterSet.CultOfUmordhoth

      if houseBurnedDown
        then startAt rivertown
        else startAt =<< place Locations.yourHouse

      count' <- getPlayerCount
      let acolytes = replicate (count' - 1) Enemies.acolyte
      for_ (zip acolytes [southside, downtown, graveyard])
        $ uncurry enemyAt

      whenHasRecord GhoulPriestIsStillAlive $ addToEncounterDeck (Only Enemies.ghoulPriest)
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- select $ NearestEnemy $ EnemyWithTrait Trait.Cultist
      player <- getPlayer iid
      pushIfAny closestCultists
        $ chooseOrRunOne player
        $ [ targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom 1]
          | x <- closestCultists
          ]
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- select $ EnemyWithTrait Trait.Cultist
      case cultists of
        [] -> push $ DrawAnotherChaosToken iid
        xs -> pushAll [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget eid) Doom 1 | eid <- xs]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      push
        $ byDifficulty
          attrs
          (InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 2)
          (InvestigatorPlaceAllCluesOnLocation iid (ChaosTokenEffectSource Tablet))
      pure s
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution n) -> do
      victoryDisplay <- selectMap toCardCode (VictoryDisplayCardMatch AnyCard)
      let
        resolution = if n == 1 then resolution1 else resolution2
        cultistsWeInterrogated = allCultists `List.intersect` victoryDisplay
        cultistsWhoGotAway = allCultists \\ cultistsWeInterrogated
        ghoulPriestDefeated = toCardCode Enemies.ghoulPriest `elem` victoryDisplay
      story resolution
      recordSetInsert CultistsWeInterrogated cultistsWeInterrogated
      recordSetInsert CultistsWhoGotAway cultistsWhoGotAway
      when (n == 2) $ record ItIsPastMidnight
      when ghoulPriestDefeated $ crossOut GhoulPriestIsStillAlive
      allGainXp attrs
      endOfScenario
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        case option of
          AddLitaChantler -> do
            investigators <- allInvestigators
            forceAddCampaignCardToDeckChoice investigators Assets.litaChantler
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheMidnightMasks <$> lift (runMessage msg attrs)
