module Arkham.Types.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Prelude

import Arkham.EncounterSet (gatherEncounterSet)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait (Trait)
import Arkham.Types.Trait qualified as Trait

newtype TheMidnightMasks = TheMidnightMasks ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasRecord env)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  TheMidnightMasks $ (baseAttrs "01120" "The Midnight Masks" [] [] difficulty)
    { scenarioLocationLayout = Just
      [ "northside downtown easttown"
      , "miskatonicUniversity rivertown graveyard"
      , "stMarysHospital southside yourHouse"
      ]
    , scenarioDecks = mapFromList [(CultistDeck, [])]
    }

instance (HasTokenValue env InvestigatorId, HasCount DoomCount env (), HasCount DoomCount env EnemyId, HasSet EnemyId env Trait) => HasTokenValue env TheMidnightMasks where
  getTokenValue (TheMidnightMasks attrs) iid = \case
    Skull | isEasyStandard attrs -> do
      cultists <- getSetList @EnemyId Trait.Cultist
      doomCounts <- traverse ((unDoomCount <$>) . getCount) cultists
      let tokenValue' = maximum $ ncons 0 doomCounts
      pure $ TokenValue Skull (NegativeModifier tokenValue')
    Skull | isHardExpert attrs -> do
      doomCount <- unDoomCount <$> getCount ()
      pure $ TokenValue Skull (NegativeModifier doomCount)
    Cultist -> pure $ TokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toTokenValue attrs Tablet 3 4
    otherFace -> getTokenValue attrs iid otherFace

data TheMidnightMasksIntroVersion = TheMidnightMasksIntroOne | TheMidnightMasksIntroTwo

introPart1 :: TheMidnightMasksIntroVersion -> Message
introPart1 version = FlavorText (Just "Part II: The MidnightMasks") body
 where
  body = case version of
    TheMidnightMasksIntroOne ->
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
    TheMidnightMasksIntroTwo ->
      [ "In the wake of the disaster at your home, Lita Chantler, the\
        \ red-haired woman from your parlor, lays out a tale that—even in light of\
        \ what you have just witnessed—strains the limits of your belief. “The creatures\
        \ in your home,” she claims, “are called ghouls—cruel beings who plague the\
        \ crypts, caverns, and tunnels beneath the city of Arkham…”"
      ]

introPart2 :: Message
introPart2 = FlavorText
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

-- "

instance ScenarioRunner env => RunMessage env TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs@ScenarioAttrs {..}) = case msg of
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
          , AddAgenda "01121"
          , AddAct "01123"
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

      pure $ TheMidnightMasks $ attrs & decksL . at CultistDeck ?~ cultistDeck'
    ResolveToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- map unClosestEnemyId
        <$> getSetList (iid, [Trait.Cultist])
      s <$ case closestCultists of
        [] -> pure ()
        [x] -> push (PlaceDoom (EnemyTarget x) 1)
        xs -> push (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
    ResolveToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- getSetList @EnemyId Trait.Cultist
      s <$ case cultists of
        [] -> push (DrawAnotherToken iid)
        xs -> pushAll [ PlaceDoom (EnemyTarget eid) 1 | eid <- xs ]
    FailedSkillTest iid _ _ (TokenTarget token) _ _
      | tokenFace token == Tablet -> if isEasyStandard attrs
        then s <$ push (InvestigatorPlaceAllCluesOnLocation iid)
        else s <$ push (InvestigatorPlaceCluesOnLocation iid 1)
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- mapSet unVictoryDisplayCardCode <$> getSet ()
      xp <- getXp
      let
        cultists =
          setFromList ["01137", "01138", "01139", "01140", "01141", "01121b"]
        cultistsWeInterrogated = cultists `intersection` victoryDisplay
        cultistsWhoGotAway = cultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                (Just "Resolution 1")
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
            <> [ GainXP iid n | (iid, n) <- xp ]
            <> [EndOfGame Nothing]
          ]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- mapSet unVictoryDisplayCardCode <$> getSet ()
      xp <- getXp
      let
        cultists =
          setFromList ["01137", "01138", "01139", "01140", "01141", "01121b"]
        cultistsWeInterrogated = cultists `intersection` victoryDisplay
        cultistsWhoGotAway = cultists `difference` cultistsWeInterrogated
        ghoulPriestDefeated = "01116" `elem` victoryDisplay
      s <$ push
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                (Just "Resolution 2")
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
            <> [ GainXP iid n | (iid, n) <- xp ]
            <> [EndOfGame Nothing]
          ]
        )
    _ -> TheMidnightMasks <$> runMessage msg attrs
