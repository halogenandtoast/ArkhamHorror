module Arkham.Conversion where

import Arkham.Internal.Scenario
import Arkham.Internal.Types
import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe

toInternalScenario :: ArkhamGame -> ArkhamScenarioInternal
toInternalScenario g =
  fromJustNote "missing scenario"
    $ HashMap.lookup (asScenarioCode scenario') allScenarios
    <*> pure difficulty'
 where
  scenario' = g ^. scenario
  difficulty' = g ^. difficulty

toInternalToken :: ArkhamGame -> ArkhamChaosToken -> ArkhamChaosTokenInternal
toInternalToken g ct = fromJustNote "token not in scenario"
  $ HashMap.lookup ct tokenMap'
 where
  scenario' = toInternalScenario g
  tokenMap' = tokenMap scenario'

tokenToModifier :: ArkhamGame -> ArkhamInvestigator -> ArkhamChaosToken -> Int
tokenToModifier game' investigator' token' = case result of
  Modifier n -> n
  Failure -> 0
 where
  result = tokenToResult
    (toInternalToken game' token')
    (game' ^. currentData . gameState)
    investigator'
