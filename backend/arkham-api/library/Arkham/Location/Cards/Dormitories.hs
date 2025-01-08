module Arkham.Location.Cards.Dormitories where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dormitories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (FastPlayerWindow)

newtype Dormitories = Dormitories LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories :: LocationCard Dormitories
dormitories = location Dormitories Cards.dormitories 1 (PerPlayer 3)

instance HasModifiersFor Dormitories where
  getModifiersFor (Dormitories a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities Dormitories where
  getAbilities (Dormitories attrs) =
    extendRevealed1 attrs
      $ restrictedAbility attrs 1 Here
      $ Objective
      $ FastAbility
      $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Dormitories")

instance RunMessage Dormitories where
  runMessage msg l@(Dormitories attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R2
      pure l
    _ -> Dormitories <$> liftRunMessage msg attrs
