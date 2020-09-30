{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ArkhamWoodsTangledThicket where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ArkhamWoodsTangledThicket = ArkhamWoodsTangledThicket Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arkhamWoodsTangledThicket :: ArkhamWoodsTangledThicket
arkhamWoodsTangledThicket =
  ArkhamWoodsTangledThicket $ (baseAttrs
                                "01154"
                                "Arkham Woods: Tangled Thicket"
                                2
                                (PerPlayer 1)
                                Square
                                [Squiggle]
                              )
    { locationTraits = HashSet.fromList [Woods]
    , locationRevealedConnectedSymbols = HashSet.fromList [Squiggle, T, Moon]
    , locationRevealedSymbol = Equals
    }

instance (IsInvestigator investigator) => HasActions env investigator ArkhamWoodsTangledThicket where
  getActions i window (ArkhamWoodsTangledThicket attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsTangledThicket where
  runMessage msg (ArkhamWoodsTangledThicket attrs@Attrs {..}) = case msg of
    Investigate iid lid _ modifiers' tokenResponses overrides False | lid == locationId ->
      ArkhamWoodsTangledThicket
        <$> runMessage
              (Investigate iid lid SkillCombat modifiers' tokenResponses overrides False)
              attrs
    _ -> ArkhamWoodsTangledThicket <$> runMessage msg attrs
