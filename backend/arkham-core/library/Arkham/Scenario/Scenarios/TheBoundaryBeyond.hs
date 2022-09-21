module Arkham.Scenario.Scenarios.TheBoundaryBeyond
  ( TheBoundaryBeyond(..)
  , theBoundaryBeyond
  ) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheBoundaryBeyond.Story
import Arkham.Token
import Arkham.Trait ( Trait (Ancient) )

newtype TheBoundaryBeyond = TheBoundaryBeyond ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBoundaryBeyond :: Difficulty -> TheBoundaryBeyond
theBoundaryBeyond difficulty = scenario
  TheBoundaryBeyond
  "04161"
  "The Boundary Beyond"
  difficulty
  [ ".        .        .    circle  circle   .      .      ."
  , "triangle triangle star star    diamond diamond square square"
  , ".        .        .    heart   heart   .       .      ."
  ]

instance HasTokenValue TheBoundaryBeyond where
  getTokenValue iid tokenFace (TheBoundaryBeyond attrs) = case tokenFace of
    Skull -> do
      atAncientLocation <-
        selectAny $ LocationWithTrait Ancient <> locationWithInvestigator iid
      let n = if atAncientLocation then 2 else 0
      pure $ toTokenValue attrs Skull (1 + n) (2 + n)
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 4 4
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheBoundaryBeyond where
  runMessage msg s@(TheBoundaryBeyond attrs) = case msg of
    Setup -> do
      iids <- getInvestigatorIds
      forgedABondWithIchtaca <- getHasRecord
        TheInvestigatorsForgedABondWithIchtaca
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      withGasoline <- headMay <$> getInvestigatorsWithSupply Gasoline

      tokens <- getTokensInBag
      let
        cultistCount = count ((== Cultist) . tokenFace) tokens
        tabletCount = count ((== Tablet) . tokenFace) tokens
        additionalSets =
          (if cultistCount >= 2
              then [EncounterSet.PnakoticBrotherhood, EncounterSet.DarkCult]
              else []
            )
            <> (if tabletCount >= 2
                 then [EncounterSet.YigsVenom, EncounterSet.GuardiansOfTime]
                 else []
               )
            <> (if cultistCount < 2 && tabletCount < 3
                 then
                   [ EncounterSet.PnakoticBrotherhood
                   , EncounterSet.GuardiansOfTime
                   ]
                 else []
               )

      encounterDeck <-
        buildEncounterDeck
        $ [ EncounterSet.TheBoundaryBeyond
          , EncounterSet.TemporalFlux
          , EncounterSet.Poison
          ]
        <> additionalSets

      pushAll
        $ [ story iids introPart1
          , story iids
            $ if forgedABondWithIchtaca then ichtacasQuest else silentJourney
          , story iids
            $ if foundTheMissingRelic then arcaneThrumming else growingConcern
          , story iids
            $ if rescuedAlejandro then alejandrosThoughts else anEmptySeat
          ]
        <> [ story iids outOfGas | isNothing withGasoline ]
        <> [ UseSupply iid Gasoline | iid <- maybeToList withGasoline ]
        <> [story iids introPart2]
        <> [SetEncounterDeck encounterDeck]
      pure s
    _ -> TheBoundaryBeyond <$> runMessage msg attrs
