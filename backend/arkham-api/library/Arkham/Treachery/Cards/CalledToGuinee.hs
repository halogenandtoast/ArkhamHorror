module Arkham.Treachery.Cards.CalledToGuinee (calledToGuinee) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CalledToGuinee = CalledToGuinee TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calledToGuinee :: TreacheryCard CalledToGuinee
calledToGuinee = treachery CalledToGuinee Cards.calledToGuinee

instance HasModifiersFor CalledToGuinee where
  getModifiersFor (CalledToGuinee attrs) = inThreatAreaGets attrs [CannotHealDamage]

instance HasAbilities CalledToGuinee where
  getAbilities (CalledToGuinee a) =
    [restricted a 1 (InThreatAreaOf You) $ actionAbilityWithCost (HandDiscardCost 3 #any)]

instance RunMessage CalledToGuinee where
  runMessage msg t@(CalledToGuinee attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CalledToGuinee <$> liftRunMessage msg attrs
