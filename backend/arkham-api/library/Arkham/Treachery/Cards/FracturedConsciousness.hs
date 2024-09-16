module Arkham.Treachery.Cards.FracturedConsciousness (
  fracturedConsciousness,
  FracturedConsciousness (..),
)
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FracturedConsciousness = FracturedConsciousness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fracturedConsciousness :: TreacheryCard FracturedConsciousness
fracturedConsciousness = treachery FracturedConsciousness Cards.fracturedConsciousness

instance RunMessage FracturedConsciousness where
  runMessage msg t@(FracturedConsciousness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #tablet attrs attrs failSkillTest
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> FracturedConsciousness <$> liftRunMessage msg attrs
