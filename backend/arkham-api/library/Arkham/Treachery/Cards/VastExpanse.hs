module Arkham.Treachery.Cards.VastExpanse (vastExpanse) where

import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VastExpanse = VastExpanse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vastExpanse :: TreacheryCard VastExpanse
vastExpanse = treachery VastExpanse Cards.vastExpanse

instance RunMessage VastExpanse where
  runMessage msg t@(VastExpanse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      extradimensionalCount <- selectCount $ LocationWithTrait Extradimensional
      if extradimensionalCount == 0
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower
            $ MaxCalculation (Fixed 5) (CountLocations $ LocationWithTrait Extradimensional)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      pure t
    _ -> VastExpanse <$> liftRunMessage msg attrs
