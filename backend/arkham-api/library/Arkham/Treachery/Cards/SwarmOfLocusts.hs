module Arkham.Treachery.Cards.SwarmOfLocusts (swarmOfLocusts) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SwarmOfLocusts = SwarmOfLocusts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swarmOfLocusts :: TreacheryCard SwarmOfLocusts
swarmOfLocusts = treachery SwarmOfLocusts Cards.swarmOfLocusts

instance RunMessage SwarmOfLocusts where
  runMessage msg t@(SwarmOfLocusts attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      resources <- field InvestigatorResources iid
      let lost = min n resources
      when (lost > 0) $ loseResources iid attrs lost
      when (n - lost > 0) $ assignDamage iid attrs (n - lost)
      pure t
    _ -> SwarmOfLocusts <$> liftRunMessage msg attrs
