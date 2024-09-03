module Arkham.Treachery.Cards.InternalInjury (internalInjury, InternalInjury (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InternalInjury = InternalInjury TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

internalInjury :: TreacheryCard InternalInjury
internalInjury = treachery InternalInjury Cards.internalInjury

instance HasAbilities InternalInjury where
  getAbilities (InternalInjury x) =
    [ restrictedAbility x 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    , restrictedAbility x 2 OnSameLocation $ ActionAbility [] $ ActionCost 2
    ]

instance RunMessage InternalInjury where
  runMessage msg t@(InternalInjury attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> InternalInjury <$> liftRunMessage msg attrs
