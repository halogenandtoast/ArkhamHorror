{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsTangledThicket where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsTangledThicket = ArkhamWoodsTangledThicket Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsTangledThicket :: ArkhamWoodsTangledThicket
arkhamWoodsTangledThicket = ArkhamWoodsTangledThicket $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, T, Moon]
  , locationRevealedSymbol = Equals
  }
 where
  base = baseAttrs
    "01154"
    "Arkham Woods: Tangled Thicket"
    2
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsTangledThicket where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ArkhamWoodsTangledThicket where
  getActions i window (ArkhamWoodsTangledThicket attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsTangledThicket where
  runMessage msg (ArkhamWoodsTangledThicket attrs@Attrs {..}) = case msg of
    Investigate iid lid s _ m tr o False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillCombat m tr o False
      ArkhamWoodsTangledThicket <$> runMessage investigate attrs
    _ -> ArkhamWoodsTangledThicket <$> runMessage msg attrs
