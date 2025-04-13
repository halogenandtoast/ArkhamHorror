module Arkham.Location.Cards.FreightCar (freightCar) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FreightCar = FreightCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

freightCar :: LocationCard FreightCar
freightCar = locationWith FreightCar Cards.freightCar 1 (PerPlayer 2) connectsToAdjacent

instance HasModifiersFor FreightCar where
  getModifiersFor (FreightCar l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities FreightCar where
  getAbilities (FreightCar a) =
    extendRevealed1 a $ restricted a 1 (exists $ investigatorAt a) $ forced $ RoundEnds #when

instance RunMessage FreightCar where
  runMessage msg l@(FreightCar attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      xs <- select $ investigatorAt attrs
      for_ xs \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 1)
      pure l
    FailedSkillTest
      iid
      _
      (isAbilitySource attrs 1 -> True)
      (SkillTestInitiatorTarget (LabeledTarget "closeToVortex" _))
      _
      _ -> do
      kill (attrs.ability 1) iid
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      sid <- getRandom
      beginSkillTest
        sid
        iid
        (attrs.ability 1)
        (LabeledTarget "closeToVortex" (toTarget iid))
        #agility
        (Fixed 1)
      pure l
    _ -> FreightCar <$> liftRunMessage msg attrs
