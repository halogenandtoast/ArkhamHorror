module Arkham.Treachery.Cards.RuinAndDestruction (ruinAndDestruction) where

import Arkham.Matcher
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RuinAndDestruction = RuinAndDestruction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinAndDestruction :: TreacheryCard RuinAndDestruction
ruinAndDestruction = treachery RuinAndDestruction Cards.ruinAndDestruction

instance RunMessage RuinAndDestruction where
  runMessage msg t@(RuinAndDestruction attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      investigators <- select $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTitle broodTitle
      if null investigators
        then gainSurge attrs
        else for_ investigators \iid -> do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs n
      pure t
    _ -> RuinAndDestruction <$> liftRunMessage msg attrs
