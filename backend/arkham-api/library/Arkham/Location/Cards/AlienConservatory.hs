module Arkham.Location.Cards.AlienConservatory (alienConservatory) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype AlienConservatory = AlienConservatory LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienConservatory :: LocationCard AlienConservatory
alienConservatory = location AlienConservatory Cards.alienConservatory 2 (PerPlayer 1)

instance HasModifiersFor AlienConservatory where
  getModifiersFor (AlienConservatory a) = do
    modifySelect a (investigatorAt a) [HandSize (-2)]

instance HasAbilities AlienConservatory where
  getAbilities (AlienConservatory a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage AlienConservatory where
  runMessage msg l@(AlienConservatory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 1 $ attrs.ability 1) attrs #willpower (Fixed 1)
      pure l
    PassedThisSkillTest iid (IndexedSource n (isAbilitySource attrs 1 -> True)) -> do
      case n of
        1 -> do
          sid <- getRandom
          beginSkillTest sid iid (IndexedSource 2 $ attrs.ability 1) attrs #agility (Fixed 2)
        2 -> do
          sid <- getRandom
          beginSkillTest sid iid (IndexedSource 3 $ attrs.ability 1) attrs #intellect (Fixed 3)
        3 -> remember SawAFamiliarSpecimen
        _ -> error "invalid index"
      pure l
    _ -> AlienConservatory <$> liftRunMessage msg attrs
