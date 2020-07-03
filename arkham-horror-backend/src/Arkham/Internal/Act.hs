module Arkham.Internal.Act
  ( ArkhamActInternal(..)
  , toInternalAct
  , lookupAct
  )
where

import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Internal.Location
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Lens.Micro
import Lens.Micro.Platform ()
import Lens.Micro.Extras (view)
import Safe (fromJustNote)

data ArkhamActInternal = ArkhamActInternal
  { actSequence :: Text
  , actCardCode :: ArkhamCardCode
  , actCanProgress :: ArkhamGameState -> Bool
  , actOnProgress :: forall m. MonadIO m => ArkhamGameState -> m ArkhamGameState
  }

toInternalAct :: ArkhamAct -> ArkhamActInternal
toInternalAct ArkhamAct {..} = lookupAct aactCardCode

lookupAct :: ArkhamCardCode -> ArkhamActInternal
lookupAct c =
  fromJustNote
      ("Could not find act for card code " <> unpack (unArkhamCardCode c))
    $ HashMap.lookup c allActs

allActs :: HashMap ArkhamCardCode ArkhamActInternal
allActs = HashMap.fromList $ map (\a -> (actCardCode a, a)) [trapped]

trapped :: ArkhamActInternal
trapped = ArkhamActInternal
  { actSequence = "Act 1a"
  , actCardCode = ArkhamCardCode "01108"
  , actCanProgress = \g ->
    getSum (foldMap (Sum . view clues) (g ^. players)) >= 2
  , actOnProgress = \g ->
      let investigators' = fromMaybe [] (g ^? locations . ix (ArkhamCardCode "01111") . investigators)
      in pure $ g & locations .~ toLocations
      [ ArkhamCardCode "01112"
      , ArkhamCardCode "01114"
      , ArkhamCardCode "01113"
      , ArkhamCardCode "01115"
      ] & locations . at (ArkhamCardCode "01112") . _Just . investigators .~ investigators'
  }
  where
    toLocations :: [ArkhamCardCode] -> HashMap ArkhamCardCode ArkhamLocation
    toLocations = HashMap.fromList . map (\c -> (c, initLocation c))
