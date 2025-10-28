module Arkham.Treachery.Cards.CelestialShower (celestialShower) where

import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CelestialShower = CelestialShower TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celestialShower :: TreacheryCard CelestialShower
celestialShower = treachery CelestialShower Cards.celestialShower

instance RunMessage CelestialShower where
  runMessage msg t@(CelestialShower attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      combinationSkillTestEdit sid iid attrs iid [#intellect, #agility] (Fixed 6) \st -> st {skillTestIsRevelation = True}
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      n <- selectCount $ colocatedWith iid
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      investigators <- select $ colocatedWith iid
      chooseOrRunOneM iid $ targets investigators \iid' -> assignDamage iid' (attrs.ability 1) 1
      doStep (n - 1) msg'
      pure t
    _ -> CelestialShower <$> liftRunMessage msg attrs
