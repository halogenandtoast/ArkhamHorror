module Arkham.Location.Cards.TheArcade (theArcade) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheArcade = TheArcade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArcade :: LocationCard TheArcade
theArcade = symbolLabel $ location TheArcade Cards.theArcade 2 (PerPlayer 1)

instance HasAbilities TheArcade where
  getAbilities (TheArcade a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage TheArcade where
  runMessage msg l@(TheArcade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainClues iid (attrs.ability 1) 1
      pure l
    _ -> TheArcade <$> liftRunMessage msg attrs
