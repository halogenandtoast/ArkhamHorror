module Arkham.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Prelude

import Arkham.Scenarios.TheMidnightMasks.Story
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher (EnemyMatcher(..), ExtendedCardMatcher(..), CardMatcher(..))
import Arkham.Message
import Arkham.Projection
import Arkham.Query
import Arkham.Resolution
import Arkham.Enemy.Attrs
import Arkham.Scenario.Attrs
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Target
import Arkham.Token
import Arkham.Trait qualified as Trait

newtype TheMidnightMasks = TheMidnightMasks ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasRecord env)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  TheMidnightMasks $ (baseAttrs "01120" "The Midnight Masks" difficulty)
    { scenarioLocationLayout = Just
      [ "northside downtown easttown"
      , "miskatonicUniversity rivertown graveyard"
      , "stMarysHospital southside yourHouse"
      ]
    , scenarioDecks = mapFromList [(CultistDeck, [])]
    }

instance (HasTokenValue env InvestigatorId, HasCount DoomCount env (), Projection env EnemyAttrs, Query EnemyMatcher env) => HasTokenValue env TheMidnightMasks where
  getTokenValue iid tokenFace (TheMidnightMasks attrs) = case tokenFace of
    Skull | isEasyStandard attrs -> do
      tokenValue' <- selectAgg max EnemyDoom (EnemyWithTrait Trait.Cultist)
      pure $ TokenValue Skull (NegativeModifier tokenValue')
    Skull | isHardExpert attrs -> do
      doomCount <- unDoomCount <$> getCount ()
      pure $ TokenValue Skull (NegativeModifier doomCount)
    Cultist -> pure $ TokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toTokenValue attrs Tablet 3 4
    otherFace -> getTokenValue iid otherFace attrs

allCultists :: HashSet CardCode
allCultists = setFromList ["01137", "01138", "01139", "01140", "01141", "01121b"]

instance ScenarioRunner env => RunMessage TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = case msg of
    Setup -> do
      count' <- getPlayerCount
      investigatorIds <- getInvestigatorIds
      (acolytes, darkCult) <- splitAt (count' - 1)
        <$> gatherEncounterSet EncounterSet.DarkCult
      -- we will spawn these acolytes

      yourHouse <- genCard Locations.yourHouse
      rivertown <- genCard Locations.rivertown
      southside <- genCard =<< sample
        (Locations.southsideHistoricalSociety
        :| [Locations.southsideMasBoardingHouse]
        )
      stMarysHospital <- genCard Locations.stMarysHospital
      miskatonicUniversity <- genCard Locations.miskatonicUniversity
      downtown <-
        genCard
          =<< sample
                (Locations.downtownFirstBankOfArkham
                :| [Locations.downtownArkhamAsylum]
                )
      easttown <- genCard Locations.easttown
      graveyard <- genCard Locations.graveyard
      northside <- genCard Locations.northside

      houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      litaForcedToFindOthersToHelpHerCause <- getHasRecord
        LitaWasForcedToFindOthersToHelpHerCause
      ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest
      cultistDeck' <-
        shuffleM
        . map EncounterCard
        =<< gatherEncounterSet EncounterSet.CultOfUmordhoth

      let
        startingLocationMessages = if houseBurnedDown
          then
            [ RevealLocation Nothing $ toLocationId rivertown
            , MoveAllTo (toSource attrs) $ toLocationId rivertown
            ]
          else
            [ PlaceLocation yourHouse
            , RevealLocation Nothing $ toLocationId yourHouse
            , MoveAllTo (toSource attrs) $ toLocationId yourHouse
            ]
        ghoulPriestMessages =
          [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
        spawnAcolyteMessages =
          [ CreateEnemyAt (EncounterCard c) l Nothing
          | (c, l) <- zip
            acolytes
            (map toLocationId [southside, downtown, graveyard])
          ]

      encounterDeck <- buildEncounterDeckWith
        (<> darkCult)
        [ EncounterSet.TheMidnightMasks
        , EncounterSet.ChillingCold
        , EncounterSet.Nightgaunts
        , EncounterSet.LockedDoors
        ]

      let
        intro1or2 = if litaForcedToFindOthersToHelpHerCause
          then TheMidnightMasksIntroOne
          else TheMidnightMasksIntroTwo

      pushAllEnd
        $ [ story investigatorIds (introPart1 intro1or2)
          , story investigatorIds introPart2
          , SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , PlaceLocation rivertown
          , PlaceLocation southside
          , PlaceLocation stMarysHospital
          , PlaceLocation miskatonicUniversity
          , PlaceLocation downtown
          , PlaceLocation easttown
          , PlaceLocation graveyard
          , PlaceLocation northside
          ]
        <> startingLocationMessages
        <> ghoulPriestMessages
        <> spawnAcolyteMessages

      TheMidnightMasks <$> runMessage
        msg
        (attrs
        & (decksL . at CultistDeck ?~ cultistDeck')
        & (actStackL . at 1 ?~ [Acts.uncoveringTheConspiracy])
        & (agendaStackL
          . at 1
          ?~ [Agendas.predatorOrPrey, Agendas.timeIsRunningShort]
          )
        )
    ResolveToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- map unClosestEnemyId
        <$> getSetList (iid, [Trait.Cultist])
      s <$ case closestCultists of
        [] -> pure ()
        [x] -> push (PlaceDoom (EnemyTarget x) 1)
        xs -> push (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
    ResolveToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- selectList $ EnemyWithTrait Trait.Cultist
      s <$ case cultists of
        [] -> push (DrawAnotherToken iid)
        xs -> pushAll [ PlaceDoom (EnemyTarget eid) 1 | eid <- xs ]
    FailedSkillTest iid _ _ (TokenTarget token) _ _
      | tokenFace token == Tablet -> if isEasyStandard attrs
        then s <$ push (InvestigatorPlaceAllCluesOnLocation iid)
        else s <$ push (InvestigatorPlaceCluesOnLocation iid 1)
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution n) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- mapSet toCardCode
        <$> select (VictoryDisplayCardMatch AnyCard)
      xp <- getXp
      let
        resolution = if n == 1 then resolution1 else resolution2
        cultistsWeInterrogated = allCultists `intersection` victoryDisplay
        cultistsWhoGotAway = allCultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , resolution
              , RecordSet
                CultistsWeInterrogated
                (setToList cultistsWeInterrogated)
              , RecordSet CultistsWhoGotAway (setToList cultistsWhoGotAway)
              ]
            <> [ Record ItIsPastMidnight | n == 2 ]
            <> [ CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated ]
            <> [ GainXP iid x | (iid, x) <- xp ]
            <> [EndOfGame Nothing]
          ]
        )
    _ -> TheMidnightMasks <$> runMessage msg attrs
