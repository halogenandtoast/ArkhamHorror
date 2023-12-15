module Arkham.Act.Cards.KingdomOfTheSkai (KingdomOfTheSkai (..), kingdomOfTheSkai) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Prelude
import Arkham.Trait (Trait (Port))

newtype KingdomOfTheSkai = KingdomOfTheSkai ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

kingdomOfTheSkai :: ActCard KingdomOfTheSkai
kingdomOfTheSkai =
  act
    (1, A)
    KingdomOfTheSkai
    Cards.kingdomOfTheSkai
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTrait Port))

instance RunMessage KingdomOfTheSkai where
  runMessage msg (KingdomOfTheSkai attrs) = KingdomOfTheSkai <$> runMessage msg attrs
