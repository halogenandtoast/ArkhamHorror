module Arkham.Types.Location.Cards.ArkhamWoodsTangledThicket where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsTangledThicket)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype ArkhamWoodsTangledThicket = ArkhamWoodsTangledThicket LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTangledThicket :: LocationId -> ArkhamWoodsTangledThicket
arkhamWoodsTangledThicket =
  ArkhamWoodsTangledThicket
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, T, Moon])
    . (revealedSymbolL .~ Equals)
    . baseAttrs
        Cards.arkhamWoodsTangledThicket
        2
        (PerPlayer 1)
        Square
        [Squiggle]

instance HasModifiersFor env ArkhamWoodsTangledThicket where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsTangledThicket where
  getActions i window (ArkhamWoodsTangledThicket attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsTangledThicket where
  runMessage msg (ArkhamWoodsTangledThicket attrs@LocationAttrs {..}) =
    case msg of
      Investigate iid lid s _ False | lid == locationId -> do
        let investigate = Investigate iid lid s SkillCombat False
        ArkhamWoodsTangledThicket <$> runMessage investigate attrs
      _ -> ArkhamWoodsTangledThicket <$> runMessage msg attrs
