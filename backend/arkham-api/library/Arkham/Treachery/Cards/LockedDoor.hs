module Arkham.Treachery.Cards.LockedDoor (lockedDoor) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedDoor :: TreacheryCard LockedDoor
lockedDoor = treachery LockedDoor Cards.lockedDoor

instance HasModifiersFor LockedDoor where
  getModifiersFor (LockedDoor attrs) = case attrs.placement of
    AttachedToLocation lid -> modified_ attrs lid [CannotInvestigate]
    _ -> pure mempty

instance HasAbilities LockedDoor where
  getAbilities (LockedDoor a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage LockedDoor where
  runMessage msg t@(LockedDoor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        (LocationWithMostClues $ LocationWithoutTreachery $ treacheryIs Cards.lockedDoor)
        \locations -> chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) attrs kind (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LockedDoor <$> liftRunMessage msg attrs
