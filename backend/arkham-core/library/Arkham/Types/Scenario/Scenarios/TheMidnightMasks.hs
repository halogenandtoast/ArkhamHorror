{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.EncounterCard (lookupEncounterCard)
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait hiding (Trait(Expert))
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import Data.UUID.V4
import System.Random.Shuffle

newtype TheMidnightMasks = TheMidnightMasks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  TheMidnightMasks $ (baseAttrs "01120" "The Midnight Masks" [] [] difficulty)
    { scenarioLocationLayout = Just
      [ "northside downtown easttown"
      , "miskatonicUniversity rivertown graveyard"
      , "stMarysHospital southside yourHouse"
      ]
    }

instance (ScenarioRunner env) => RunMessage env TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs@Attrs {..}) = case msg of
    Setup -> do
      count <- unPlayerCount <$> asks (getCount ())
      (acolytes, darkCult) <- splitAt (count - 1)
        <$> gatherEncounterSet EncounterSet.DarkCult
      -- ^ we will spawn these acolytes
      southside <- liftIO $ sample $ "01126" :| ["01127"]
      downtown <- liftIO $ sample $ "01130" :| ["01131"]
      houseBurnedDown <- asks $ hasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- asks $ hasRecord GhoulPriestIsStillAlive
      ghoulPriestCard <-
        liftIO $ lookupEncounterCard "01116" . CardId <$> nextRandom
      let
        startingLocationMessages = if houseBurnedDown
          then [RevealLocation "01125", MoveAllTo "01125"]
          else
            [PlaceLocation "01124", RevealLocation "01124", MoveAllTo "01124"]
        ghoulPriestMessages =
          if ghoulPriestAlive then [AddToEncounterDeck ghoulPriestCard] else []
        spawnAcolyteMessages =
          [ CreateEnemyAt (getCardCode c) l
          | (c, l) <- zip acolytes [southside, downtown, "01133"]
          ]
      encounterDeck <- liftIO $ shuffleM . (<> darkCult) . concat =<< traverse
        gatherEncounterSet
        [ EncounterSet.TheMidnightMasks
        , EncounterSet.ChillingCold
        , EncounterSet.Nightgaunts
        , EncounterSet.DarkCult
        , EncounterSet.LockedDoors
        ]
      s <$ pushMessages
        ([ SetEncounterDeck encounterDeck
         , AddAgenda "01121"
         , AddAct "01123"
         , PlaceLocation "01125"
         , PlaceLocation southside
         , PlaceLocation "01128"
         , PlaceLocation "01129"
         , PlaceLocation downtown
         , PlaceLocation "01132"
         , PlaceLocation "01133"
         , PlaceLocation "01134"
         ]
        <> startingLocationMessages
        <> ghoulPriestMessages
        <> spawnAcolyteMessages
        )
    ResolveToken Token.Skull _ skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        cultists <- HashSet.toList <$> asks (getSet @EnemyId Cultist)
        doomCounts <- map unDoomCount <$> traverse (asks . getCount) cultists
        s <$ runTest skillValue (-(maximum $ ncons 0 doomCounts))
    ResolveToken Token.Skull _ skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        doomCount <- unDoomCount <$> asks (getCount ())
        s <$ runTest skillValue (-doomCount)
    ResolveToken Token.Cultist iid skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        closestCultitsts <- map unClosestEnemyId . HashSet.toList <$> asks
          (getSet (iid, [Cultist]))
        case closestCultitsts of
          [] -> pure ()
          [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) 1)
          xs -> unshiftMessage
            (Ask iid $ ChooseOne [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
        s <$ runTest skillValue (-2)
    ResolveToken Token.Cultist iid skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        cultists <- HashSet.toList <$> asks (getSet @EnemyId Cultist)
        case cultists of
          [] ->
            s <$ unshiftMessage
              (DrawAnotherToken iid skillValue Token.Cultist (-2))
          xs -> do
            unshiftMessages [ PlaceDoom (EnemyTarget eid) 1 | eid <- xs ]
            s <$ runTest skillValue (-2)

    ResolveToken Token.Tablet iid skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        unshiftMessage (AddOnFailure $ InvestigatorPlaceCluesOnLocation iid 1)
        s <$ runTest skillValue (-3)
    ResolveToken Token.Tablet iid skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        unshiftMessage (AddOnFailure $ InvestigatorPlaceAllCluesOnLocation iid)
        s <$ runTest skillValue (-4)
    NoResolution -> s <$ unshiftMessage (Resolution 1)
    Resolution 1 -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      victoryDisplay <- HashSet.map unVictoryDisplayCardCode
        <$> asks (getSet ())
      let
        cultists = HashSet.fromList
          ["01137", "01138", "01139", "01140", "01141", "01121b"]
        cultistsWeInterrogated = cultists `intersection` victoryDisplay
        cultistsWhoGotAway = cultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ unshiftMessage
        (Ask leadInvestigatorId $ ChooseOne
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You’ve managed to obtain some useful\
                  \ information about the cult and its plans. You can only hope\
                  \ it’s enough."
                ]
              , RecordSet
                CultistsWeInterrogated
                (HashSet.toList cultistsWeInterrogated)
              , RecordSet CultistsWhoGotAway (HashSet.toList cultistsWhoGotAway)
              ]
            <> [ CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated ]
            <> [EndOfGame]
          ]
        )
    Resolution 2 -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      victoryDisplay <- HashSet.map unVictoryDisplayCardCode
        <$> asks (getSet ())
      let
        cultists = HashSet.fromList
          ["01137", "01138", "01139", "01140", "01141", "01121b"]
        cultistsWeInterrogated = cultists `intersection` victoryDisplay
        cultistsWhoGotAway = cultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ unshiftMessage
        (Ask leadInvestigatorId $ ChooseOne
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "Twelve bells ring out, signaling midnight. You’re\
                  \ out of time; the cult’s ritual will begin shortly. You’ve managed\
                  \ to obtain some useful information about the cult and its plans.\
                  \ You can only hope it’s enough."
                ]
              , RecordSet
                CultistsWeInterrogated
                (HashSet.toList cultistsWeInterrogated)
              , RecordSet CultistsWhoGotAway (HashSet.toList cultistsWhoGotAway)
              , Record IsIsPastMidnight
              ]
            <> [ CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated ]
            <> [EndOfGame]
          ]
        )
    _ -> TheMidnightMasks <$> runMessage msg attrs
