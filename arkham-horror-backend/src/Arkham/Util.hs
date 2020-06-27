module Arkham.Util where

import Arkham.Internal.Types
import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Database.Persist.Sql
import GHC.Stack
import Lens.Micro
import Safe

drawCard :: ArkhamGameData -> ArkhamGameData
drawCard g =
  let (drawn, deck') = splitAt 1 (g ^. player . deck)
  in g & player . hand %~ (++ drawn) & player . deck .~ deck'

updateGame
  :: (MonadIO m) => ArkhamGameId -> ArkhamGame -> SqlPersistT m ArkhamGameData
updateGame gameId game = replace gameId game $> arkhamGameCurrentData game

token :: HasCallStack => ArkhamChaosTokenInternal
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
