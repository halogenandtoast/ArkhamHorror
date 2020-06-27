module Arkham.Util where

import Arkham.Internal.Types
import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe

token :: ArkhamChaosTokenInternal
token = ArkhamChaosTokenInternal
  { tokenToResult = error "you must specify a result"
  , tokenOnFail = const
  , tokenOnSuccess = const
  , tokenOnReveal = const
  }

locationFor :: ArkhamInvestigator -> ArkhamGameState -> ArkhamLocation
locationFor investigator' g =
  fromJustNote "the investigator appears to be nowhere"
    $ find (investigatorIsAtLocation investigator')
    $ HashMap.elems (g ^. locations)

investigatorIsAtLocation :: ArkhamInvestigator -> ArkhamLocation -> Bool
investigatorIsAtLocation investigator' = elem investigator' . alInvestigators

countTraitMatch :: ArkhamCardTrait -> ArkhamLocation -> Int
countTraitMatch _ _ = 0
