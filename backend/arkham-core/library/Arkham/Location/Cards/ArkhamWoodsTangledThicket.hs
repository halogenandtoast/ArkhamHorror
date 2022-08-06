module Arkham.Location.Cards.ArkhamWoodsTangledThicket where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsTangledThicket )
import Arkham.Location.Runner
import Arkham.Message
import Arkham.SkillType

newtype ArkhamWoodsTangledThicket = ArkhamWoodsTangledThicket LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsTangledThicket :: LocationCard ArkhamWoodsTangledThicket
arkhamWoodsTangledThicket = location
  ArkhamWoodsTangledThicket
  Cards.arkhamWoodsTangledThicket
  2
  (PerPlayer 1)

-- TODO: Move this to a modifier
instance RunMessage ArkhamWoodsTangledThicket where
  runMessage msg (ArkhamWoodsTangledThicket attrs@LocationAttrs {..}) =
    case msg of
      Investigate iid lid s mt _ False | lid == locationId -> do
        let investigate = Investigate iid lid s mt SkillCombat False
        ArkhamWoodsTangledThicket <$> runMessage investigate attrs
      _ -> ArkhamWoodsTangledThicket <$> runMessage msg attrs
