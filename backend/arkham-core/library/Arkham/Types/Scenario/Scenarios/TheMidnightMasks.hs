module Arkham.Types.Scenario.Scenarios.TheMidnightMasks where


import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import Arkham.Types.Trait (Trait)
import qualified Arkham.Types.Trait as Trait
import Data.List.NonEmpty (NonEmpty(..))

newtype TheMidnightMasks = TheMidnightMasks ScenarioAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  TheMidnightMasks $ (baseAttrs "01120" "The Midnight Masks" [] [] difficulty)
    { scenarioLocationLayout = Just
      [ "northside downtown easttown"
      , "miskatonicUniversity rivertown graveyard"
      , "stMarysHospital southside yourHouse"
      ]
    , scenarioDeck = Just $ CultistDeck []
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

instance ScenarioRunner env => RunMessage env TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs@ScenarioAttrs {..}) = case msg of
    Setup -> do
      count' <- getPlayerCount
      investigatorIds <- getInvestigatorIds
      (acolytes, darkCult) <- splitAt (count' - 1)
        <$> gatherEncounterSet EncounterSet.DarkCult
      -- ^ we will spawn these acolytes
      southside <- sample $ "01126" :| ["01127"]
      downtown <- sample $ "01130" :| ["01131"]
      houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      litaForcedToFindOthersToHelpHerCause <- getHasRecord
        LitaWasForcedToFindOthersToHelpHerCause
      ghoulPriestCard <- lookupEncounterCard "01116" <$> getRandom
      cultistCards <- for ["01137", "01138", "01139", "01140", "01141"]
        $ \cardCode -> lookupEncounterCard cardCode <$> getRandom
      cultistDeck' <- shuffleM cultistCards
      let
        startingLocationMessages = if houseBurnedDown
          then [RevealLocation Nothing "01125", MoveAllTo "01125"]
          else
            [ PlaceLocation "01124"
            , RevealLocation Nothing "01124"
            , MoveAllTo "01124"
            ]
        ghoulPriestMessages =
          [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
        spawnAcolyteMessages =
          [ CreateEnemyAt (EncounterCard c) l
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
          then TheMidnightMasksIntroOne
          else TheMidnightMasksIntroTwo
      pushMessages
        $ [ story investigatorIds (introPart1 intro1or2)
          , story investigatorIds introPart2
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
      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocation))
          [ "01124"
          , "01124"
          , southside
          , "01128"
          , "01129"
          , downtown
          , "01132"
          , "01133"
          , "01134"
          ]
      pure $ TheMidnightMasks
        (attrs
          { scenarioDeck = Just $ CultistDeck cultistDeck'
          , scenarioLocations = locations'
          }
        )
    UseScenarioSpecificAbility iid 1 ->
      case fromJustNote "must be set" scenarioDeck of
        CultistDeck [] -> pure s
        CultistDeck (x : xs) -> do
          unshiftMessage (InvestigatorDrewEncounterCard iid x)
          pure $ TheMidnightMasks
            (attrs { scenarioDeck = Just $ CultistDeck xs })
        _ -> error "Wrong deck"
    ResolveToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- map unClosestEnemyId
        <$> getSetList (iid, [Trait.Cultist])
      s <$ case closestCultists of
        [] -> pure ()
        [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) 1)
        xs -> unshiftMessage
          (chooseOne iid [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
    ResolveToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- getSetList @EnemyId Trait.Cultist
      s <$ case cultists of
        [] -> unshiftMessage (DrawAnotherToken iid)
        xs -> unshiftMessages [ PlaceDoom (EnemyTarget eid) 1 | eid <- xs ]
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _
      | drawnTokenFace token == Tablet -> if isEasyStandard attrs
        then s <$ unshiftMessage (InvestigatorPlaceAllCluesOnLocation iid)
        else s <$ unshiftMessage (InvestigatorPlaceCluesOnLocation iid 1)
    ScenarioResolution NoResolution ->
      s <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- mapSet unVictoryDisplayCardCode <$> getSet ()
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
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      victoryDisplay <- mapSet unVictoryDisplayCardCode <$> getSet ()
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
