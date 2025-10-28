module Arkham.Location.Cards.BuenosAires (buenosAires) where

import Arkham.Ability
import Arkham.Helpers.Doom
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier), modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BuenosAires = BuenosAires LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buenosAires :: LocationCard BuenosAires
buenosAires = location BuenosAires Cards.buenosAires 5 (Static 3)

instance HasModifiersFor BuenosAires where
  getModifiersFor (BuenosAires a) = do
    doom <- getDoom a.id
    modifySelfWhen a (doom > 0) [ShroudModifier (doom * (-2))]

instance HasAbilities BuenosAires where
  getAbilities (BuenosAires a) = extendRevealed1 a $ mkAbility a 1 $ forced $ PhaseEnds #when #mythos

instance RunMessage BuenosAires where
  runMessage msg l@(BuenosAires attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> BuenosAires <$> liftRunMessage msg attrs
