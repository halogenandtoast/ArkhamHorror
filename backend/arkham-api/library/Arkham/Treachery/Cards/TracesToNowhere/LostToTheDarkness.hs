module Arkham.Treachery.Cards.TracesToNowhere.LostToTheDarkness (lostToTheDarkness) where

import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostToTheDarkness = LostToTheDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostToTheDarkness :: TreacheryCard LostToTheDarkness
lostToTheDarkness = treachery LostToTheDarkness Cards.lostToTheDarkness

instance RunMessage LostToTheDarkness where
  runMessage msg t@(LostToTheDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- length <$> getAbductedCards
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed n)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> LostToTheDarkness <$> liftRunMessage msg attrs
