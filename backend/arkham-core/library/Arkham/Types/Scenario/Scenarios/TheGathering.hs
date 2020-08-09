{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheGathering where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import Arkham.Types.Source
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait
import ClassyPrelude
import System.Random.Shuffle

newtype TheGathering = TheGathering Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theGathering :: Difficulty -> TheGathering
theGathering = TheGathering . baseAttrs
  "01104"
  "The Gathering"
  ["01105", "01106", "01107"]
  ["01108", "01109", "01110"]

instance (ScenarioRunner env) => RunMessage env TheGathering where
  runMessage msg s@(TheGathering attrs@Attrs {..}) = case msg of
    Setup -> do
      encounterDeck <- liftIO $ shuffleM . concat =<< traverse
        gatherEncounterSet
        [ EncounterSet.TheGathering
        , EncounterSet.Rats
        , EncounterSet.Ghouls
        , EncounterSet.StrikingFear
        , EncounterSet.ChillingCold
        ]
      pushMessages
        [ SetEncounterDeck encounterDeck
        , AddAgenda "01105"
        , AddAct "01108"
        , PlaceLocation "01111"
        , RevealLocation "01111"
        , MoveAllTo "01111"
        , Ask $ ChooseOne
          [ Run
              [ Continue "Continue"
              , FlavorText
                (Just "Part I: The Gathering")
                "You and your partners have been investigating strange events taking place\
              \ in your home city of Arkham, Massachusetts. Over the past few weeks,\
              \ several townspeople have mysteriously gone missing. Recently, their\
              \ corpses turned up in the woods, savaged and half - eaten. The police and\
              \ newspapers have stated that wild animals are responsible, but you believe\
              \ there is something else going on. You are gathered together at the lead\
              \ investigator’s home to discuss these bizarre events."
              ]
          ]
        ]
      TheGathering <$> runMessage msg attrs
    ResolveToken Token.Skull iid skillValue ->
      if scenarioDifficulty `elem` [Easy, Standard]
        then do
          ghoulCount <- unEnemyCount
            <$> asks (getCount (InvestigatorLocation iid, [Ghoul]))
          s <$ runTest (skillValue - ghoulCount)
        else do
          unshiftMessage
            (AddOnFailure $ FindAndDrawEncounterCard iid (EnemyType, Ghoul))
          s <$ runTest (skillValue - 2)
    ResolveToken Token.Cultist iid skillValue ->
      if scenarioDifficulty `elem` [Easy, Standard]
        then do
          unshiftMessage
            (AddOnFailure
            $ InvestigatorAssignDamage iid (TokenSource Token.Cultist) 0 1
            )
          s <$ runTest (skillValue - 1)
        else do
          unshiftMessage (DrawAnotherToken iid skillValue Token.Cultist)
          unshiftMessage
            (AddOnFailure
            $ InvestigatorAssignDamage iid (TokenSource Token.Cultist) 0 2
            )
          pure s
    ResolveToken Token.Tablet iid skillValue -> do
      ghoulCount <- unEnemyCount
        <$> asks (getCount (InvestigatorLocation iid, [Ghoul]))
      if scenarioDifficulty `elem` [Easy, Standard]
        then do
          when (ghoulCount > 0) $ unshiftMessage
            (InvestigatorAssignDamage iid (TokenSource Token.Tablet) 1 0)
          s <$ runTest (skillValue - 2)
        else do
          when (ghoulCount > 0) $ unshiftMessage
            (InvestigatorAssignDamage iid (TokenSource Token.Tablet) 1 1)
          s <$ runTest (skillValue - 4)
    NoResolution -> s <$ unshiftMessages
      [ Continue "Continue"
      , FlavorText
        Nothing
        "You barely manage to escape\
        \ your house with your lives. The woman from your parlor\
        \ follows you out the front door, slamming it behind her. “You\
        \ fools! See what you have done?” She pushes a chair in front of\
        \ the door, lodging it beneath the doorknob. “We must get out\
        \ of here. Come with me, and I will tell you what I know. We\
        \ are the only ones who can stop the threat that lurks beneath\
        \ from being unleashed throughout the city.” You’re in no state\
        \ to argue. Nodding, you follow the woman as she runs from\
        \ your front porch out into the rainy street, toward Rivertown."
      , GameOver
      ]
    Resolution 1 -> s <$ unshiftMessages
      [ Continue "Continue"
      , FlavorText
        Nothing
        "You nod and allow the red-haired woman to\
        \ set the walls and floor of your house ablaze. The fire spreads\
        \ quickly, and you run out the front door to avoid being caught\
        \ in the inferno. From the sidewalk, you watch as everything\
        \ you own is consumed by the flames. “Come with me,” the\
        \ woman says. “You must be told of the threat that lurks below.\
        \ Alone, we are surely doomed…but together, we can stop it.”"
      , GameOver
      ]
    Resolution 2 -> s <$ unshiftMessages
      [ Continue "Continue"
      , FlavorText
        Nothing
        "You refuse to follow the overzealous woman’s\
        \ order and kick her out of your home for fear that she will set\
        \ it ablaze without your permission. “Fools! You are making\
        \ a grave mistake!” she warns. “You do not understand the\
        \ threat that lurks below…the grave danger we are all in!”\
        \ Still shaken by the night’s events, you decide to hear the\
        \ woman out. Perhaps she can shed some light on these bizarre\
        \ events…but she doesn’t seem to trust you very much."
      , GameOver
      ]
    Resolution 3 -> s <$ unshiftMessages
      [ Continue "Continue"
      , FlavorText
        Nothing
        "You run to the hallway to try to find a way to\
        \ escape the house, but the burning-hot barrier still blocks your\
        \ path. Trapped, the horde of feral creatures that have invaded\
        \ your home close in, and you have nowhere to run."
      , GameOver
      ]
    _ -> pure s

