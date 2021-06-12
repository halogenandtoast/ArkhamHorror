module Arkham.Types.Location.Cards.ArkhamWoodsTangledThicket where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype ArkhamWoodsTangledThicket = ArkhamWoodsTangledThicket LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTangledThicket :: LocationId -> ArkhamWoodsTangledThicket
arkhamWoodsTangledThicket =
  ArkhamWoodsTangledThicket
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, T, Moon])
    . (revealedSymbolL .~ Equals)
    . baseAttrs
        "01154"
        ("Arkham Woods" `subtitled` "Tangled Thicket")
        EncounterSet.TheDevourerBelow
        2
        (PerPlayer 1)
        Square
        [Squiggle]
        [Woods]

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
