module Arkham.Internal.Act where

import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Lens.Micro
import Lens.Micro.Extras (view)
import Safe (fromJustNote)

data ArkhamActInternal = ArkhamActInternal
  { actSequence :: Text
  , actCardCode :: ArkhamCardCode
  , actCanProgress :: ArkhamGameState -> Bool
  , actOnProgress :: ArkhamGameState -> ArkhamGameState
  }

toInternalAct :: ArkhamAct -> ArkhamActInternal
toInternalAct ArkhamAct {..} =
  fromJustNote
      ("Could not find act for card code "
      <> unpack (unArkhamCardCode aactCardCode)
      )
    $ HashMap.lookup aactCardCode allActs

allActs :: HashMap ArkhamCardCode ArkhamActInternal
allActs = HashMap.fromList $ map (\a -> (actCardCode a, a)) [trapped]

trapped :: ArkhamActInternal
trapped = ArkhamActInternal
  { actSequence = "Act 1a"
  , actCardCode = ArkhamCardCode "01108"
  , actCanProgress = \g ->
    getSum (foldMap (Sum . view clues) [g ^. player]) >= 2
  , actOnProgress = id
  }
