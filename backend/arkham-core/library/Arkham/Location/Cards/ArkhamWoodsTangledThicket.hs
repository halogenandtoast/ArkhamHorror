module Arkham.Location.Cards.ArkhamWoodsTangledThicket where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (arkhamWoodsTangledThicket)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Message
import Arkham.SkillType

newtype ArkhamWoodsTangledThicket = ArkhamWoodsTangledThicket LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsTangledThicket :: LocationCard ArkhamWoodsTangledThicket
arkhamWoodsTangledThicket = locationWithRevealedSideConnections
  ArkhamWoodsTangledThicket
  Cards.arkhamWoodsTangledThicket
  2
  (PerPlayer 1)
  Square
  [Squiggle]
  Equals
  [Squiggle, T, Moon]

-- TODO: Move this to a modifier
instance (LocationRunner env) => RunMessage env ArkhamWoodsTangledThicket where
  runMessage msg (ArkhamWoodsTangledThicket attrs@LocationAttrs {..}) =
    case msg of
      Investigate iid lid s mt _ False | lid == locationId -> do
        let investigate = Investigate iid lid s mt SkillCombat False
        ArkhamWoodsTangledThicket <$> runMessage investigate attrs
      _ -> ArkhamWoodsTangledThicket <$> runMessage msg attrs
