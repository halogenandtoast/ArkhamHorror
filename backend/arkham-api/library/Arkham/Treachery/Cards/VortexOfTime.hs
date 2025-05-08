module Arkham.Treachery.Cards.VortexOfTime (vortexOfTime) where

import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VortexOfTime = VortexOfTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vortexOfTime :: TreacheryCard VortexOfTime
vortexOfTime = treachery VortexOfTime Cards.vortexOfTime

instance RunMessage VortexOfTime where
  runMessage msg t@(VortexOfTime attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ LocationWithTrait SentinelHill) \iid -> do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> VortexOfTime <$> liftRunMessage msg attrs
