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

meetsOrExceeds
  :: ArkhamValue -> (ArkhamGameState -> Int) -> ArkhamGameState -> Bool
meetsOrExceeds ct f g = let t = clueThreshold ct g in t > -1 && f g >= t

clueThreshold :: ArkhamValue -> ArkhamGameState -> Int
clueThreshold (Static n) _ = n
clueThreshold (PerInvestigator n) g = n * length (g ^. players)
clueThreshold Blank _ = -1

act :: ArkhamCardCode -> Text -> ArkhamValue -> ArkhamActInternal
act code' sequence' clueThreshold' = ArkhamActInternal
  { actSequence = sequence'
  , actCardCode = code'
  , actCanProgress = meetsOrExceeds clueThreshold' totalClues
  , actOnProgress = pure
  }

-- brittany-disable-next-binding

trapped :: ArkhamActInternal
trapped = (act (ArkhamCardCode "01108") "Act 1a" clueThreshold')
  { actOnProgress = \g -> do
    let investigators' = g ^. locations . ix "01111" . investigators
        clueCount = clueThreshold clueThreshold' g
    foldM discardEnemy g (g ^. locations . ix "01111" . enemyIds)
      <&> locations .~ toLocations [ "01112" , "01114" , "01113" , "01115" ]
      <&> locations . ix "01112" . investigators .~ investigators'
      <&> activePlayer . clues -~ clueCount -- TODO: players can spend clues however they want
      <&> (over (locations . ix "01112") =<< aliOnReveal hallway)
  }
 where
  clueThreshold' = PerInvestigator 2
  toLocations = HashMap.fromList . map (\c -> (c, initLocation c))
  hallway = lookupLocationInternal "01112"

theBarrier :: ArkhamActInternal
theBarrier = act "01109" "Act 2a" (PerInvestigator 3)
