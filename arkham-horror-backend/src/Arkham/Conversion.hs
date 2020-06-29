module Arkham.Conversion
  ( toInternalToken
  , tokenToModifier
  )
where

import Arkham.Entity.ArkhamGame
import Arkham.Internal.Scenario
import Arkham.Internal.Types
import Arkham.Types
import Arkham.Types.ChaosToken
import Arkham.Types.Investigator
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe

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
