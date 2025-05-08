module Arkham.Location.Cards.IndecipherableStairs (indecipherableStairs) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.LostInTimeAndSpace.Helpers

newtype IndecipherableStairs = IndecipherableStairs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indecipherableStairs :: LocationCard IndecipherableStairs
indecipherableStairs = location IndecipherableStairs Cards.indecipherableStairs 3 (Static 1)

instance HasAbilities IndecipherableStairs where
  getAbilities (IndecipherableStairs a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Leaves #after You (be a)

instance RunMessage IndecipherableStairs where
  runMessage msg l@(IndecipherableStairs attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discardTopOfDeck iid attrs 3
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        withI18n $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 2
        scenarioI18n $ labeled "indecipherableStairs.discard" $ toDiscardBy iid (attrs.ability 1) attrs
      pure l
    _ -> IndecipherableStairs <$> liftRunMessage msg attrs
