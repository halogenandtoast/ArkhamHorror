module Arkham.Treachery.Cards.CrumblingMasonry (crumblingMasonry) where

import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Location.Types (Field (LocationPrintedShroud))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrumblingMasonry = CrumblingMasonry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingMasonry :: TreacheryCard CrumblingMasonry
crumblingMasonry = treachery CrumblingMasonry Cards.crumblingMasonry

instance RunMessage CrumblingMasonry where
  runMessage msg t@(CrumblingMasonry attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      difficulty <- runMaybeT do
        lid <- MaybeT $ getMaybeLocation iid
        value <- MaybeT $ field LocationPrintedShroud lid
        lift $ getGameValue value
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ fromMaybe 0 difficulty)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> CrumblingMasonry <$> liftRunMessage msg attrs
