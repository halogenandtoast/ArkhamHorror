module Arkham.Treachery.Cards.ChildrenOfBlood.Hunted.OfTheNight (ofTheNight) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Hunted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OfTheNight = OfTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ofTheNight :: TreacheryCard OfTheNight
ofTheNight = treachery OfTheNight Cards.ofTheNight

instance RunMessage OfTheNight where
  runMessage msg t@(OfTheNight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> OfTheNight <$> liftRunMessage msg attrs
