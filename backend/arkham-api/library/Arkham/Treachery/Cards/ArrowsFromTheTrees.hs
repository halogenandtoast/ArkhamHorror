module Arkham.Treachery.Cards.ArrowsFromTheTrees (arrowsFromTheTrees) where

import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArrowsFromTheTrees = ArrowsFromTheTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arrowsFromTheTrees :: TreacheryCard ArrowsFromTheTrees
arrowsFromTheTrees = treachery ArrowsFromTheTrees Cards.arrowsFromTheTrees

instance RunMessage ArrowsFromTheTrees where
  runMessage msg t@(ArrowsFromTheTrees attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      others <- select $ not_ (be iid) <> InvestigatorAt (LocationWithTrait Ancient)
      for_ (iid : others) \iid' -> do
        allyCount <- selectCount $ assetControlledBy iid' <> #ally
        assignDamage iid' attrs (allyCount + 1)
      pure t
    _ -> ArrowsFromTheTrees <$> liftRunMessage msg attrs
