module Arkham.Treachery.Cards.EyesInTheWalls (eyesInTheWalls) where

import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyesInTheWalls = EyesInTheWalls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheWalls :: TreacheryCard EyesInTheWalls
eyesInTheWalls = treachery EyesInTheWalls Cards.eyesInTheWalls

instance RunMessage EyesInTheWalls where
  runMessage msg t@(EyesInTheWalls attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageEvenly 0 n
      pure t
    _ -> EyesInTheWalls <$> liftRunMessage msg attrs
