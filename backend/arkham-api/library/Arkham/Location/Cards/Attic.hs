module Arkham.Location.Cards.Attic (attic) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Attic = Attic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attic :: LocationCard Attic
attic = location Attic Cards.attic 1 (PerPlayer 2)

instance HasAbilities Attic where
  getAbilities (Attic a) = extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage Attic where
  runMessage msg a@(Attic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    _ -> Attic <$> liftRunMessage msg attrs
