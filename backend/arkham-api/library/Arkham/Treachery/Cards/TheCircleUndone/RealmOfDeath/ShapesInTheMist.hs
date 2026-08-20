module Arkham.Treachery.Cards.TheCircleUndone.RealmOfDeath.ShapesInTheMist (shapesInTheMist) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Treachery.CardDefs.TheCircleUndone.RealmOfDeath qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShapesInTheMist = ShapesInTheMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shapesInTheMist :: TreacheryCard ShapesInTheMist
shapesInTheMist = treachery ShapesInTheMist Cards.shapesInTheMist

instance RunMessage ShapesInTheMist where
  runMessage msg t@(ShapesInTheMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      runHauntedAbilities iid
      pure t
    _ -> ShapesInTheMist <$> liftRunMessage msg attrs
