{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsOldHouse where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsOldHouse = ArkhamWoodsOldHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsOldHouse :: ArkhamWoodsOldHouse
arkhamWoodsOldHouse = ArkhamWoodsOldHouse $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Triangle, T]
  , locationRevealedSymbol = Diamond
  }
 where
  base = baseAttrs
    "01152"
    "Arkham Woods: Old House"
    EncounterSet.TheDevourerBelow
    2
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsOldHouse where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsOldHouse where
  getActions i window (ArkhamWoodsOldHouse attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsOldHouse where
  runMessage msg (ArkhamWoodsOldHouse attrs@Attrs {..}) = case msg of
    Investigate iid lid s _ m tr o False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillWillpower m tr o False
      ArkhamWoodsOldHouse <$> runMessage investigate attrs
    _ -> ArkhamWoodsOldHouse <$> runMessage msg attrs
