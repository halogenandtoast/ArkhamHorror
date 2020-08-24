{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheDevourerBelow where

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.EncounterCard
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
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait hiding (Trait(Expert))
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import Data.UUID.V4
import System.Random.Shuffle

newtype TheDevourerBelow = TheDevourerBelow Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theDevourerBelow :: Difficulty -> TheDevourerBelow
theDevourerBelow difficulty =
  TheDevourerBelow $ (baseAttrs "01142" "The Devourer Below" [] [] difficulty)
    { scenarioLocationLayout = Just
      [ "woods1     .     woods2"
      , "woods1 mainPath woods2"
      , "woods3 mainPath woods4"
      , "woods3 ritualSite woods4"
      , "   .   ritualSite   .  "
      ]
    }

instance (ScenarioRunner env) => RunMessage env TheDevourerBelow where
  runMessage msg s@(TheDevourerBelow attrs@Attrs {..}) = case msg of
    Setup -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      pastMidnight <- asks $ hasRecord ItIsPastMidnight
      ghoulPriestAlive <- asks $ hasRecord GhoulPriestIsStillAlive
      cultistsWhoGotAway <- asks $ hasRecordSet CultistsWhoGotAway
      ghoulPriestCard <-
        liftIO $ lookupEncounterCard "01116" . CardId <$> nextRandom
      let
        arkhamWoods = ["01150", "01151", "01152", "01153", "01154", "01155"]
        woodsLabels = ["woods1", "woods2", "woods3", "woods4"]
        ghoulPriestMessages =
          if ghoulPriestAlive then [AddToEncounterDeck ghoulPriestCard] else []
        pastMidnightMessages =
          if pastMidnight then [AllRandomDiscard, AllRandomDiscard] else []
        cultistsWhoGotAwayMessages =
          replicate ((length cultistsWhoGotAway + 1) `div` 2) PlaceDoomOnAgenda
      woodsLocations <- liftIO $ take 4 <$> shuffleM arkhamWoods
      randomSet <-
        liftIO
        . sample
        $ EncounterSet.AgentsOfYogSothoth
        :| [ EncounterSet.AgentsOfShubNiggurath
           , EncounterSet.AgentsOfCthulhu
           , EncounterSet.AgentsOfHastur
           ]
      encounterDeck <- liftIO $ shuffleM . concat =<< traverse
        gatherEncounterSet
        [ EncounterSet.TheDevourerBelow
        , EncounterSet.AncientEvils
        , EncounterSet.StrikingFear
        , EncounterSet.Ghouls
        , EncounterSet.DarkCult
        , randomSet
        ]
      pushMessages
        $ [ AskMap
            (HashMap.fromList
              [ ( iid
                , ChooseOne
                  [ Run
                      [ Continue "Continue"
                      , FlavorText
                        (Just "Part III: The Devourer Below")
                        [ "After a frantic nighttime search throughout Arkham, you have tracker\
                              \ down and questioned several members of the cult. Your findings are\
                              \ disturbing: they claim to worship a being known as Umôrdhoth, a\
                              \ monstrous entity from another realm."
                        , "You are able to confirm much of Lita’s story: the cult is agitated over\
                              \ the destruction of a ghoul lair. However, a surprising detail also turns\
                              \ up: the one who invaded the lair and set this night’s events in motion\
                              \ was none other than Lita Chantler herself! You are not sure why this\
                              \ important detail was omitted from Lita’s story—did she tell you only\
                              \ as much as was necessary to draw you into her conflict? But in another\
                              \ light, she seems to be fighting to protect the city of Arkham from a\
                              \ terrible menace."
                        , "The final piece of the puzzle was found written in a journal possessed by\
                              \ one of the cultists. It describes a dark ritual to be performed deep within\
                              \ the woods south of Arkham, this very night. According to the journal,\
                              \ the ritual’s completion will open a gate and bring forth the cult’s dark\
                              \ master into this world. “If the cult is not stopped,” Lita warns, “there is\
                              \ a possibility that Umôrdhoth’s vengeance will consume all in its path.”\
                              \ Frightened but determined to stop the ritual, you head into the woods…"
                        ]
                      ]
                  ]
                )
              | iid <- investigatorIds
              ]
            )
          , SetEncounterDeck encounterDeck
          , AddToken Token.ElderThing
          , AddAgenda "01143"
          , AddAct "01146"
          , PlaceLocation "01149"
          ]
        <> [ PlaceLocation location | location <- woodsLocations ]
        <> [ SetLocationLabel location label
           | (location, label) <- zip woodsLocations woodsLabels
           ]
        <> [RevealLocation "01149", MoveAllTo "01149"]
        <> ghoulPriestMessages
        <> cultistsWhoGotAwayMessages
        <> pastMidnightMessages
      pure s
    ResolveToken Token.Skull _ skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        monsterCount <- unEnemyCount <$> asks (getCount [Monster])
        s <$ runTest skillValue (-monsterCount)
    ResolveToken Token.Skull iid skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        unshiftMessage
          (AddOnFailure $ FindAndDrawEncounterCard iid (EnemyType, Just Monster)
          )
        s <$ runTest skillValue (-3)
    ResolveToken Token.Cultist iid skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        closestEnemyIds <- map unClosestEnemyId . HashSet.toList <$> asks
          (getSet iid)
        case closestEnemyIds of
          [] -> pure ()
          [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) 1)
          xs -> unshiftMessage
            (Ask iid $ ChooseOne [ PlaceDoom (EnemyTarget x) 1 | x <- xs ])
        s <$ runTest skillValue (-2)
    ResolveToken Token.Cultist iid skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        closestEnemyIds <- map unClosestEnemyId . HashSet.toList <$> asks
          (getSet iid)
        case closestEnemyIds of
          [] -> pure ()
          [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) 2)
          xs -> unshiftMessage
            (Ask iid $ ChooseOne [ PlaceDoom (EnemyTarget x) 2 | x <- xs ])
        s <$ runTest skillValue (-4)
    ResolveToken Token.Tablet iid skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        ghoulCount <- unEnemyCount
          <$> asks (getCount (InvestigatorLocation iid, [Monster]))
        when (ghoulCount > 0) $ unshiftMessage
          (InvestigatorAssignDamage iid (TokenSource Token.Tablet) 1 0)
        s <$ runTest skillValue (-3)
    ResolveToken Token.Tablet iid skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        ghoulCount <- unEnemyCount
          <$> asks (getCount (InvestigatorLocation iid, [Monster]))
        when (ghoulCount > 0) $ unshiftMessage
          (InvestigatorAssignDamage iid (TokenSource Token.Tablet) 1 1)
        s <$ runTest skillValue (-5)
    ResolveToken Token.ElderThing iid skillValue
      | scenarioDifficulty `elem` [Easy, Standard] -> do
        ancientOneCount <- unEnemyCount <$> asks (getCount [AncientOne])
        if ancientOneCount > 0
          then
            s <$ unshiftMessage
              (DrawAnotherToken iid skillValue Token.ElderThing (-5))
          else s <$ runTest skillValue (-5)
    ResolveToken Token.ElderThing iid skillValue
      | scenarioDifficulty `elem` [Hard, Expert] -> do
        ancientOneCount <- unEnemyCount <$> asks (getCount [AncientOne])
        if ancientOneCount > 0
          then
            s <$ unshiftMessage
              (DrawAnotherToken iid skillValue Token.ElderThing (-7))
          else s <$ runTest skillValue (-5)
    NoResolution -> error "not yet implemented"
    Resolution 1 -> error "not yet implemented"
    Resolution 2 -> error "not yet implemented"
    Resolution 3 -> error "not yet implemented"
    _ -> TheDevourerBelow <$> runMessage msg attrs
