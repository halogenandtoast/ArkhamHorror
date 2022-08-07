module Arkham.Location.Cards.ArkhamWoodsOldHouse where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( arkhamWoodsOldHouse )
import Arkham.Location.Runner
import Arkham.Message
import Arkham.SkillType

newtype ArkhamWoodsOldHouse = ArkhamWoodsOldHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsOldHouse :: LocationCard ArkhamWoodsOldHouse
arkhamWoodsOldHouse =
  location ArkhamWoodsOldHouse Cards.arkhamWoodsOldHouse 2 (PerPlayer 1)

-- TODO: Move this to a modifier
instance RunMessage ArkhamWoodsOldHouse where
  runMessage msg (ArkhamWoodsOldHouse attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s mt _ False | lid == locationId -> do
      let investigate = Investigate iid lid s mt SkillWillpower False
      ArkhamWoodsOldHouse <$> runMessage investigate attrs
    _ -> ArkhamWoodsOldHouse <$> runMessage msg attrs
