module Arkham.Treachery.Cards.DraggedUnder where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DraggedUnder = DraggedUnder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

draggedUnder :: TreacheryCard DraggedUnder
draggedUnder = treachery DraggedUnder Cards.draggedUnder

instance HasAbilities DraggedUnder where
  getAbilities (DraggedUnder x) =
    [ restrictedAbility x 1 (InThreatAreaOf You) $ forced $ Leaves #when You Anywhere
    , skillTestAbility $ restrictedAbility x 2 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    ]

instance RunMessage DraggedUnder where
  runMessage msg t@(DraggedUnder attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      assignDamage iid source 2
      toDiscardBy iid source attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      when (isNothing attrs.attached) (placeInThreatArea attrs iid)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> DraggedUnder <$> liftRunMessage msg attrs
