module Arkham.Treachery.Cards.ChildrenOfBlood.BloodBlight.BlightedBlood (blightedBlood) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodBlight qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BlightedBlood = BlightedBlood TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blightedBlood :: TreacheryCard BlightedBlood
blightedBlood = treachery BlightedBlood Cards.blightedBlood

instance RunMessage BlightedBlood where
  runMessage msg t@(BlightedBlood attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BlightedBlood <$> liftRunMessage msg attrs
