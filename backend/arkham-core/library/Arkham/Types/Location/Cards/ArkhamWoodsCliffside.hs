{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsCliffside :: ArkhamWoodsCliffside
arkhamWoodsCliffside = ArkhamWoodsCliffside $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Moon, Triangle]
  , locationRevealedSymbol = Hourglass
  }
 where
  base = baseAttrs
    "01153"
    "Arkham Woods: Cliffside"
    EncounterSet.TheDevourerBelow
    2
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsCliffside where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsCliffside where
  getActions i window (ArkhamWoodsCliffside attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs@Attrs {..}) = case msg of
    Investigate iid lid s _ m tr o False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillAgility m tr o False
      ArkhamWoodsCliffside <$> runMessage investigate attrs
    _ -> ArkhamWoodsCliffside <$> runMessage msg attrs
