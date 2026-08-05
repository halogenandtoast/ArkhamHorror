module Arkham.Treachery.Cards.LayWaste (layWaste) where

import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LayWaste = LayWaste TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

layWaste :: TreacheryCard LayWaste
layWaste = treachery LayWaste Cards.layWaste

instance RunMessage LayWaste where
  runMessage msg t@(LayWaste attrs) = runQueueT $ case msg of
    -- "Revelation - Test [agility] (X), where X is Cthulhu's Rage. If you fail, take
    -- 2 damage."
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (ScenarioCount CthulhuRage)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> LayWaste <$> liftRunMessage msg attrs
