module Arkham.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
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
  setFromList ["01137", "01138", "01139", "01140", "01141", "01121b"]

instance RunMessage TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = case msg of
    Setup -> do
      count' <- getPlayerCount
      investigatorIds <- allInvestigatorIds
      (acolytes, darkCult) <-
        splitAt (count' - 1)
          . sortOn toCardCode
          <$> gatherEncounterSet EncounterSet.DarkCult
      -- we will spawn these acolytes

      (yourHouseId, placeYourHouse) <- placeLocationCard Locations.yourHouse
      (rivertownId, placeRivertown) <- placeLocationCard Locations.rivertown
      (southsideId, placeSouthside) <-
        placeLocationCard
          =<< sample
            ( Locations.southsideHistoricalSociety
                :| [Locations.southsideMasBoardingHouse]
            )
      (downtownId, placeDowntown) <-
        placeLocationCard
          =<< sample
            ( Locations.downtownFirstBankOfArkham
                :| [Locations.downtownArkhamAsylum]
            )
      (graveyardId, placeGraveyard) <- placeLocationCard Locations.graveyard
      placeEasttown <- placeLocationCard_ Locations.easttown
      placeMiskatonicUniversity <-
        placeLocationCard_
          Locations.miskatonicUniversity
      placeNorthside <- placeLocationCard_ Locations.northside
      placeStMarysHospital <- placeLocationCard_ Locations.stMarysHospital

      houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      litaForcedToFindOthersToHelpHerCause <-
        getHasRecord
          LitaWasForcedToFindOthersToHelpHerCause
      ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest
      cultistDeck' <-
        shuffleM
          . map EncounterCard
          =<< gatherEncounterSet EncounterSet.CultOfUmordhoth

      let
        startingLocationMessages =
          if houseBurnedDown
            then
              [ RevealLocation Nothing rivertownId
              , MoveAllTo (toSource attrs) rivertownId
              ]
            else
              [ placeYourHouse
              , RevealLocation Nothing yourHouseId
              , MoveAllTo (toSource attrs) yourHouseId
              ]
        ghoulPriestMessages =
          [AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive]

      spawnAcolyteMessages <-
        for (zip acolytes [southsideId, downtownId, graveyardId]) $
          \(c, l) -> createEnemyAt_ (EncounterCard c) l Nothing

      encounterDeck <-
        buildEncounterDeckWith
          (<> darkCult)
          [ EncounterSet.TheMidnightMasks
          , EncounterSet.ChillingCold
          , EncounterSet.Nightgaunts
          , EncounterSet.LockedDoors
          ]

      let
        intro1or2 =
          if litaForcedToFindOthersToHelpHerCause
            then TheMidnightMasksIntroOne
            else TheMidnightMasksIntroTwo

      pushAll $
        [ story investigatorIds (introPart1 intro1or2)
        , story investigatorIds introPart2
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , placeRivertown
        , placeSouthside
        , placeStMarysHospital
        , placeMiskatonicUniversity
        , placeDowntown
        , placeEasttown
        , placeGraveyard
        , placeNorthside
        ]
          <> startingLocationMessages
          <> ghoulPriestMessages
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
      closestCultists <-
        selectList $
          NearestEnemy $
            EnemyWithTrait
              Trait.Cultist
      case closestCultists of
        [] -> pure ()
        [x] -> push $ PlaceTokens (ChaosTokenEffectSource Cultist) (EnemyTarget x) Doom 1
        xs ->
          push $
            chooseOne
              iid
              [targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom 1] | x <- xs]
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- selectList $ EnemyWithTrait Trait.Cultist
      case cultists of
        [] -> push (DrawAnotherChaosToken iid)
        xs -> pushAll [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget eid) Doom 1 | eid <- xs]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      push $
        if isEasyStandard attrs
          then InvestigatorPlaceAllCluesOnLocation iid (ChaosTokenEffectSource Tablet)
          else InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1
      pure s
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution n) -> do
      iids <- allInvestigatorIds
      victoryDisplay <-
        mapSet toCardCode
          <$> select (VictoryDisplayCardMatch AnyCard)
      xp <- getXp
      let
        resolution = if n == 1 then resolution1 else resolution2
        cultistsWeInterrogated = allCultists `intersection` victoryDisplay
        cultistsWhoGotAway = allCultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      pushAll $
        [ story iids resolution
        , recordSetInsert CultistsWeInterrogated cultistsWeInterrogated
        , recordSetInsert CultistsWhoGotAway cultistsWhoGotAway
        ]
          <> [Record ItIsPastMidnight | n == 2]
          <> [CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated]
          <> [GainXP iid (toSource attrs) x | (iid, x) <- xp]
          <> [EndOfGame Nothing]
      pure s
    _ -> TheMidnightMasks <$> runMessage msg attrs
