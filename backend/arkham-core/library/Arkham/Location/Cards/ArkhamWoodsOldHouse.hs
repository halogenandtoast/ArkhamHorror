module Arkham.Location.Cards.ArkhamWoodsOldHouse where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (arkhamWoodsOldHouse)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Message
import Arkham.SkillType

newtype ArkhamWoodsOldHouse = ArkhamWoodsOldHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsOldHouse :: LocationCard ArkhamWoodsOldHouse
arkhamWoodsOldHouse = locationWithRevealedSideConnections
  ArkhamWoodsOldHouse
  Cards.arkhamWoodsOldHouse
  2
  (PerPlayer 1)
  Square
  [Squiggle]
  Diamond
  [Squiggle, Triangle, T]

-- TODO: Move this to a modifier
instance LocationRunner env => RunMessage env ArkhamWoodsOldHouse where
  runMessage msg (ArkhamWoodsOldHouse attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s mt _ False | lid == locationId -> do
      let investigate = Investigate iid lid s mt SkillWillpower False
      ArkhamWoodsOldHouse <$> runMessage investigate attrs
    _ -> ArkhamWoodsOldHouse <$> runMessage msg attrs
