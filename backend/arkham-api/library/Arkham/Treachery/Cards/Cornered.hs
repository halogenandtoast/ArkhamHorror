module Arkham.Treachery.Cards.Cornered (cornered) where

import Arkham.Helpers.Location
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Cornered = Cornered TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cornered :: TreacheryCard Cornered
cornered = treachery Cornered Cards.cornered

instance RunMessage Cornered where
  runMessage msg t@(Cornered attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid do
        scenarioI18n $ labeled' "cornered.slain" $ withLocationOf iid slayCivilian
        withI18n $ countVar n $ labeled' "takeDamage" $ assignDamage iid attrs n
      pure t
    _ -> Cornered <$> liftRunMessage msg attrs
