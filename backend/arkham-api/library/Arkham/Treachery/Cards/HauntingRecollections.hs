module Arkham.Treachery.Cards.HauntingRecollections (hauntingRecollections) where

import Arkham.Investigator.Projection ()
import Arkham.Name (toTitle)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HauntingRecollections = HauntingRecollections TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntingRecollections :: TreacheryCard HauntingRecollections
hauntingRecollections = treachery HauntingRecollections Cards.hauntingRecollections

instance RunMessage HauntingRecollections where
  runMessage msg t@(HauntingRecollections attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hand <- map toTitle <$> iid.hand
      discards <- iid.discard
      let n = min 3 $ count ((`elem` hand) . toTitle) discards
      if n == 0
        then discardTopOfDeck iid attrs 3
        else assignHorror iid attrs n
      pure t
    _ -> HauntingRecollections <$> liftRunMessage msg attrs
