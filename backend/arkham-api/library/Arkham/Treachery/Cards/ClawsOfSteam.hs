module Arkham.Treachery.Cards.ClawsOfSteam (clawsOfSteam, ClawsOfSteam (..)) where

import Arkham.Modifier
import Arkham.Source
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ClawsOfSteam = ClawsOfSteam TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clawsOfSteam :: TreacheryCard ClawsOfSteam
clawsOfSteam = treachery ClawsOfSteam Cards.clawsOfSteam

instance RunMessage ClawsOfSteam where
  runMessage msg t@(ClawsOfSteam attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      roundModifier attrs iid CannotMove
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAssetsFirst 2 0
      pure t
    _ -> ClawsOfSteam <$> liftRunMessage msg attrs
