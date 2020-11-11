{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Import hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import Arkham.Types.Trait (Trait)
import qualified Arkham.Types.Trait as Trait
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
    , scenarioDeck = Just []
    }

instance (HasTokenValue env InvestigatorId, HasQueue env, HasCount DoomCount () env, HasCount DoomCount EnemyId env, HasSet EnemyId Trait env) => HasTokenValue env TheMidnightMasks where
  getTokenValue (TheMidnightMasks attrs) iid = \case
    Skull | isEasyStandard attrs -> do
      cultists <- asks $ setToList . getSet @EnemyId Trait.Cultist
      doomCounts <- traverse (asks . (unDoomCount .) . getCount) cultists
      let tokenValue' = maximum $ ncons 0 doomCounts
      pure $ TokenValue Skull (NegativeModifier tokenValue')
    Skull | isHardExpert attrs -> do
      doomCount <- asks $ unDoomCount . getCount ()
      pure $ TokenValue Skull (NegativeModifier doomCount)
    Cultist -> pure $ TokenValue Cultist (NegativeModifier 2)
    Tablet -> do
      let tokenValue' = if isEasyStandard attrs then 3 else 4
      pure $ TokenValue Tablet (NegativeModifier tokenValue')
    otherFace -> getTokenValue attrs iid otherFace

instance (ScenarioRunner env) => RunMessage env TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs@Attrs {..}) = case msg of
    Setup -> do
      count' <- getPlayerCount
      investigatorIds <- getInvestigatorIds
      (acolytes, darkCult) <- splitAt (count' - 1)
        <$> gatherEncounterSet EncounterSet.DarkCult
      -- ^ we will spawn these acolytes
      southside <- liftIO $ sample $ "01126" :| ["01127"]
      downtown <- liftIO $ sample $ "01130" :| ["01131"]
      houseBurnedDown <- asks $ hasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- asks $ hasRecord GhoulPriestIsStillAlive
      litaForcedToFindOthersToHelpHerCause <- asks
        $ hasRecord LitaWasForcedToFindOthersToHelpHerCause
      ghoulPriestCard <-
        liftIO $ lookupEncounterCard "01116" . CardId <$> nextRandom
      cultistCards <-
        liftIO
        $ for ["01137", "01138", "01139", "01140", "01141"]
        $ \cardCode -> lookupEncounterCard cardCode . CardId <$> nextRandom
      cultistDeck' <- liftIO $ shuffleM cultistCards
      let
        startingLocationMessages = if houseBurnedDown
          then [RevealLocation Nothing "01125", MoveAllTo "01125"]
          else
            [ PlaceLocation "01124"
            , RevealLocation Nothing "01124"
            , MoveAllTo "01124"
            ]
        ghoulPriestMessages =
          if ghoulPriestAlive then [AddToEncounterDeck ghoulPriestCard] else []
        spawnAcolyteMessages =
          [ CreateEnemyAt (getCardCode c) l
          | (c, l) <- zip acolytes [southside, downtown, "01133"]
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
          then
            [ "The woman came to you in a panic, raving about monsters emerging\
                              \ from the ground in a home near Rivertown. “I managed to trap them,” she\
                              \ explains, “but there are others. Other pits. Other domains.” Only last week,\
                              \ you would have thought she was a lunatic. Recent events, however, have\
                              \ challenged your preconceptions of normality. You decide to hear her out."
            , "She introduces herself as Lita Chantler and lays out a tale that strains\
                              \ the limits of your belief. “The creatures I speak of ,” she claims, “are called\
                              \ ghouls—cruel beings who plague the crypts, caverns, and tunnels beneath the\
                              \ city of Arkham…”"
            ]
          else
            [ "In the wake of the disaster at your home, Lita Chantler, the\
                              \ red-haired woman from your parlor, lays out a tale that—even in light of\
                              \ what you have just witnessed—strains the limits of your belief. “The creatures\
                              \ in your home,” she claims, “are called ghouls—cruel beings who plague the\
                              \ crypts, caverns, and tunnels beneath the city of Arkham…”"
            ]
      pushMessages
        $ [ AskMap
            (mapFromList
              [ ( iid
                , ChooseOne
                  [ Run
                      [ Continue "Continue"
                      , FlavorText (Just "Part II: The MidnightMasks") intro1or2
                      ]
                  ]
                )
              | iid <- investigatorIds
              ]
            )
          , AskMap
            (mapFromList
              [ ( iid
                , ChooseOne
                  [ Run
                      [ Continue "Continue"
                      , FlavorText
                        (Just "Part II: The MidnightMasks")
                        [ "“These creatures feed on the corpses of humans, and they are served\
                           \ by a dark cult within Arkham whose members have inexplicably come to\
                           \ worship the ancient master of the ghouls. This cult has been killing innocent\
                           \ people and feeding them to the ghouls, satiating a monstrous hunger. A dark\
                           \ balance was maintained. Until now. Recently,” Lita continues, “one of their\
                           \ lairs, where the corpses were stored, was destroyed. Since then, the ghouls have\
                           \ been more active than usual. I have tracked their movements and tried my\
                           \ best to stop them from running amok throughout the city. But I think there\
                           \ is something worse going on. The cult has been planning something darker,\
                           \ and more ominous, than anything I have yet observed. Indications are that\
                           \ this plan shall come to fruition tonight, shortly after midnight. Beyond that, I\
                           \ cannot fathom what to expect."
                        , "“Many of the cultists,” Lita continues, “will seem like everyday people, despite\
                           \ their foul intentions. Whenever the cult meets, its members don masks shaped\
                           \ like the skulls of various animals to protect their identities from one another.\
                           \ These masks are our mark. Symbols of death and decay. We must unmask the\
                           \ cultists to expose and derail their plans. We have but a few hours. The more\
                           \ cultists we find before midnight, the better.”"
                        ]
                      ]
                  ]
                )
              | iid <- investigatorIds
              ]
            )
          , SetEncounterDeck encounterDeck
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
      pure $ TheMidnightMasks (attrs { scenarioDeck = Just cultistDeck' })
    UseScenarioSpecificAbility iid 1 ->
      case fromJustNote "must be set" scenarioDeck of
        [] -> pure s
        (x : xs) -> do
          unshiftMessage (InvestigatorDrewEncounterCard iid x)
          pure $ TheMidnightMasks (attrs { scenarioDeck = Just xs })
    ResolveToken Cultist iid | isEasyStandard attrs -> do
      closestCultists <- asks $ map unClosestEnemyId . setToList . getSet
        (iid, [Trait.Cultist])
      case closestCultists of
        [] -> pure ()
        [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) 1)
        xs -> unshiftMessage
          (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
      pure s
    ResolveToken Cultist iid | isHardExpert attrs -> do
      cultists <- asks $ setToList . getSet @EnemyId Trait.Cultist
      case cultists of
        [] -> unshiftMessage (DrawAnotherToken iid)
        xs -> unshiftMessages [ PlaceDoom (EnemyTarget eid) 1 | eid <- xs ]
      pure s
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _
      | drawnTokenFace token == Tablet -> if isEasyStandard attrs
        then s <$ unshiftMessage (InvestigatorPlaceAllCluesOnLocation iid)
        else s <$ unshiftMessage (InvestigatorPlaceCluesOnLocation iid 1)
    NoResolution -> s <$ unshiftMessage (Resolution 1)
    Resolution 1 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- asks $ HashSet.map unVictoryDisplayCardCode . getSet ()
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      let
        cultists =
          setFromList ["01137", "01138", "01139", "01140", "01141", "01121b"]
        cultistsWeInterrogated = cultists `intersection` victoryDisplay
        cultistsWhoGotAway = cultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
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
                (setToList cultistsWeInterrogated)
              , RecordSet CultistsWhoGotAway (setToList cultistsWhoGotAway)
              ]
            <> [ CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated ]
            <> [ GainXP iid xp | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 2 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- asks $ HashSet.map unVictoryDisplayCardCode . getSet ()
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      let
        cultists =
          setFromList ["01137", "01138", "01139", "01140", "01141", "01121b"]
        cultistsWeInterrogated = cultists `intersection` victoryDisplay
        cultistsWhoGotAway = cultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
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
                (setToList cultistsWeInterrogated)
              , RecordSet CultistsWhoGotAway (setToList cultistsWhoGotAway)
              , Record ItIsPastMidnight
              ]
            <> [ CrossOutRecord GhoulPriestIsStillAlive | ghoulPriestDefeated ]
            <> [ GainXP iid xp | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    _ -> TheMidnightMasks <$> runMessage msg attrs
