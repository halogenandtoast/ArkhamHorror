module Arkham.Location.Cards.MysteriousStairs_183 (
  mysteriousStairs_183,
  MysteriousStairs_183 (..),
)
where

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype MysteriousStairs_183 = MysteriousStairs_183 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_183 :: LocationCard MysteriousStairs_183
mysteriousStairs_183 =
  locationWith
    MysteriousStairs_183
    Cards.mysteriousStairs_183
    3
    (Static 0)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities MysteriousStairs_183 where
  getAbilities (MysteriousStairs_183 x) = extendRevealed x [forcedAbility x 1 $ Enters #after You $ be x]

instance RunMessage MysteriousStairs_183 where
  runMessage msg l@(MysteriousStairs_183 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ LoseActions iid (attrs.ability 1) 1
      pure l
    _ -> MysteriousStairs_183 <$> runMessage msg attrs
