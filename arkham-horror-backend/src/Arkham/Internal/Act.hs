module Arkham.Internal.Act
  ( ArkhamActInternal(..)
  , toInternalAct
  , lookupAct
  )
where

import Arkham.Internal.Location
import Arkham.Internal.Util
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.Platform ()
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
allActs =
  HashMap.fromList $ map (\a -> (actCardCode a, a)) [trapped, theBarrier]

totalClues :: ArkhamGameState -> Int
totalClues = getSum . foldMap (Sum . view clues) . view players

data ClueThreshold = Static Int | PerInvestigator Int | Blank

meetsOrExceeds
  :: ClueThreshold -> (ArkhamGameState -> Int) -> ArkhamGameState -> Bool
meetsOrExceeds ct f g = let t = clueThreshold ct g in t > -1 && f g >= t

clueThreshold :: ClueThreshold -> ArkhamGameState -> Int
clueThreshold (Static n) _ = n
clueThreshold (PerInvestigator n) g = n * length (g ^. players)
clueThreshold Blank _ = -1

act :: ArkhamCardCode -> Text -> ClueThreshold -> ArkhamActInternal
act code' sequence' clueThreshold' = ArkhamActInternal
  { actSequence = sequence'
  , actCardCode = code'
  , actCanProgress = meetsOrExceeds clueThreshold' totalClues
  , actOnProgress = pure
  }

-- TODO: discard enemies
trapped :: ArkhamActInternal
trapped = (act (ArkhamCardCode "01108") "Act 1a" clueThreshold')
  { actOnProgress = \g -> do
    let
      investigators' = fromMaybe
        mempty
        (g ^? locations . ix (ArkhamCardCode "01111") . investigators)
    g' <-
      foldM
        discardEnemy
        g
        (g ^. locations . ix (ArkhamCardCode "01108") . enemyIds)
      <&> locations
      .~ toLocations
           [ ArkhamCardCode "01112"
           , ArkhamCardCode "01114"
           , ArkhamCardCode "01113"
           , ArkhamCardCode "01115"
           ]
      <&> locations
      . at (ArkhamCardCode "01112")
      . _Just
      . investigators
      .~ investigators'
    pure
      $ g'
      & activePlayer
      . clues
      -~ clueThreshold clueThreshold' g
      & locations
      . at (ArkhamCardCode "01112")
      . mapped
      %~ aliOnReveal hallway g'
  }
 where
  clueThreshold' = PerInvestigator 2
  toLocations = HashMap.fromList . map (\c -> (c, initLocation c))
  hallway = lookupLocationInternal $ ArkhamCardCode "01112"

theBarrier :: ArkhamActInternal
theBarrier = act (ArkhamCardCode "01109") "Act 2a" (PerInvestigator 3)

