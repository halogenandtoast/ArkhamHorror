module Arkham.Treachery.Cards.CaptiveMind (captiveMind) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest (getSkillTestModifiedSkillValue)
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaptiveMind = CaptiveMind TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captiveMind :: TreacheryCard CaptiveMind
captiveMind = treachery CaptiveMind Cards.captiveMind

instance RunMessage CaptiveMind where
  runMessage msg t@(CaptiveMind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 0)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      chooseAndDiscardCards iid attrs =<< getSkillTestModifiedSkillValue
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseAndDiscardCards iid attrs =<< getSkillTestModifiedSkillValue
      pure t
    _ -> CaptiveMind <$> liftRunMessage msg attrs
