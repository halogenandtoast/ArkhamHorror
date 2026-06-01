module Arkham.Location.Cards.TheOldMillMorning (theOldMillMorning) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheOldMillMorning = TheOldMillMorning LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldMillMorning :: LocationCard TheOldMillMorning
theOldMillMorning = symbolLabel $ location TheOldMillMorning Cards.theOldMillMorning 4 (Static 1)

instance HasAbilities TheOldMillMorning where
  getAbilities (TheOldMillMorning a) =
    extendRevealed1 a $ restricted a 1 (Here <> thisExists a LocationWithAnyClues) doubleActionAbility

instance RunMessage TheOldMillMorning where
  runMessage msg l@(TheOldMillMorning attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> TheOldMillMorning <$> liftRunMessage msg attrs
