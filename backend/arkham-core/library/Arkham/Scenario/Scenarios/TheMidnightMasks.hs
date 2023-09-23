module Arkham.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLogKey
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
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheMidnightMasks.Story
import Arkham.Token
import Arkham.Trait qualified as Trait

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
    Skull | isEasyStandard attrs -> do
      tokenValue' <- fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)
      pure $ ChaosTokenValue Skull (NegativeModifier tokenValue')
    Skull | isHardExpert attrs -> do
      doomCount <- getDoomCount
      pure $ ChaosTokenValue Skull (NegativeModifier doomCount)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

allCultists :: Set CardCode
allCultists =
  setFromList
    $ map
      toCardCode
      [ Enemies.wolfManDrew
      , Enemies.hermanCollins
      , Enemies.peterWarren
      , Enemies.victoriaDevereux
      , Enemies.ruthTurner
      , Enemies.theMaskedHunter
      ]

instance RunMessage TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = case msg of
    PreScenarioSetup -> do
      investigators <- allInvestigators
      litaForcedToFindOthersToHelpHerCause <- getHasRecord LitaWasForcedToFindOthersToHelpHerCause
      pushAll
        [ story investigators
            $ introPart1
            $ if litaForcedToFindOthersToHelpHerCause then TheMidnightMasksIntroOne else TheMidnightMasksIntroTwo
        , story investigators introPart2
        ]
      pure s
    Setup -> do
      count' <- getPlayerCount
      (acolytes, darkCult) <-
        splitAt (count' - 1) . sortOn toCardCode <$> gatherEncounterSet EncounterSet.DarkCult
      -- we will spawn these acolytes

      (yourHouse, placeYourHouse) <- placeLocationCard Locations.yourHouse
      (rivertown, placeRivertown) <- placeLocationCard Locations.rivertown
      (southside, placeSouthside) <-
        placeLocationCard
          =<< sample2 Locations.southsideHistoricalSociety Locations.southsideMasBoardingHouse
      (downtown, placeDowntown) <-
        placeLocationCard =<< sample2 Locations.downtownFirstBankOfArkham Locations.downtownArkhamAsylum
      (graveyard, placeGraveyard) <- placeLocationCard Locations.graveyard
      otherPlacements <-
        placeLocationCards_
          [Locations.easttown, Locations.miskatonicUniversity, Locations.northside, Locations.stMarysHospital]

      houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest
      cultistDeck' <- shuffleM . map EncounterCard =<< gatherEncounterSet EncounterSet.CultOfUmordhoth

      let
        startingLocationMessages =
          if houseBurnedDown
            then
              [ RevealLocation Nothing rivertown
              , MoveAllTo (toSource attrs) rivertown
              ]
            else
              [ placeYourHouse
              , RevealLocation Nothing yourHouse
              , MoveAllTo (toSource attrs) yourHouse
              ]

      spawnAcolyteMessages <-
        for (zip acolytes [southside, downtown, graveyard])
          $ \(c, l) -> createEnemyAt_ (EncounterCard c) l Nothing

      encounterDeck <-
        buildEncounterDeckWith
          (<> darkCult)
          [ EncounterSet.TheMidnightMasks
          , EncounterSet.ChillingCold
          , EncounterSet.Nightgaunts
          , EncounterSet.LockedDoors
          ]

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeRivertown
          , placeSouthside
          , placeDowntown
          , placeGraveyard
          ]
        <> otherPlacements
        <> startingLocationMessages
        <> [AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive]
        <> spawnAcolyteMessages

      agendas <- genCards [Agendas.predatorOrPrey, Agendas.timeIsRunningShort]
      acts <- genCards [Acts.uncoveringTheConspiracy]

      TheMidnightMasks
        <$> runMessage
          msg
          ( attrs
              & (decksL . at CultistDeck ?~ cultistDeck')
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- selectList $ NearestEnemy $ EnemyWithTrait Trait.Cultist
      case closestCultists of
        [] -> pure ()
        [x] -> push $ PlaceTokens (ChaosTokenEffectSource Cultist) (EnemyTarget x) Doom 1
        xs ->
          push
            $ chooseOne iid
            $ [targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom 1] | x <- xs]
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- selectList $ EnemyWithTrait Trait.Cultist
      case cultists of
        [] -> push $ DrawAnotherChaosToken iid
        xs -> pushAll [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget eid) Doom 1 | eid <- xs]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      push
        $ if isEasyStandard attrs
          then InvestigatorPlaceAllCluesOnLocation iid (ChaosTokenEffectSource Tablet)
          else InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1
      pure s
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution n) -> do
      iids <- allInvestigatorIds
      victoryDisplay <- mapSet toCardCode <$> select (VictoryDisplayCardMatch AnyCard)
      gainXp <- toGainXp attrs getXp
      let
        resolution = if n == 1 then resolution1 else resolution2
        cultistsWeInterrogated = allCultists `intersection` victoryDisplay
        cultistsWhoGotAway = allCultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = toCardCode Enemies.ghoulPriest `elem` victoryDisplay
      pushAll
        $ [ story iids resolution
          , recordSetInsert CultistsWeInterrogated cultistsWeInterrogated
          , recordSetInsert CultistsWhoGotAway cultistsWhoGotAway
          ]
        <> [Record ItIsPastMidnight | n == 2]
        <> [CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated]
        <> gainXp
        <> [EndOfGame Nothing]
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        lead <- getLead
        investigators <- allInvestigators
        case option of
          AddLitaChantler -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.litaChantler
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheMidnightMasks <$> runMessage msg attrs
