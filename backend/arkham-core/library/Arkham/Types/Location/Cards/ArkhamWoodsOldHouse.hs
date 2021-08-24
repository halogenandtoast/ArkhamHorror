module Arkham.Types.Location.Cards.ArkhamWoodsOldHouse where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsOldHouse)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype ArkhamWoodsOldHouse = ArkhamWoodsOldHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

arkhamWoodsOldHouse :: LocationCard ArkhamWoodsOldHouse
arkhamWoodsOldHouse = locationWith
  ArkhamWoodsOldHouse
  Cards.arkhamWoodsOldHouse
  2
  (PerPlayer 1)
  Square
  [Squiggle]
  ((revealedConnectedSymbolsL .~ setFromList [Squiggle, Triangle, T])
  . (revealedSymbolL .~ Diamond)
  )

-- TODO: Move this to a modifier
instance LocationRunner env => RunMessage env ArkhamWoodsOldHouse where
  runMessage msg (ArkhamWoodsOldHouse attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s _ False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillWillpower False
      ArkhamWoodsOldHouse <$> runMessage investigate attrs
    _ -> ArkhamWoodsOldHouse <$> runMessage msg attrs
