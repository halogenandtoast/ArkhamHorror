module Arkham.Treachery.Cards.VastExpanse (vastExpanse, VastExpanse (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VastExpanse = VastExpanse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vastExpanse :: TreacheryCard VastExpanse
vastExpanse = treachery VastExpanse Cards.vastExpanse

instance RunMessage VastExpanse where
  runMessage msg t@(VastExpanse attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      extradimensionalCount <- selectCount $ LocationWithTrait Extradimensional
      sid <- getRandom
      push
        $ if extradimensionalCount == 0
          then gainSurge attrs
          else
            revelationSkillTest sid iid attrs #willpower
              $ MaxCalculation (Fixed 5) (CountLocations $ LocationWithTrait Extradimensional)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ assignHorror iid attrs n
      pure t
    _ -> VastExpanse <$> runMessage msg attrs
