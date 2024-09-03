module Arkham.Treachery.Cards.LockedDoor (LockedDoor (..), lockedDoor) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedDoor :: TreacheryCard LockedDoor
lockedDoor = treachery LockedDoor Cards.lockedDoor

instance HasModifiersFor LockedDoor where
  getModifiersFor (LocationTarget lid) (LockedDoor attrs) =
    modified attrs [CannotInvestigate | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities LockedDoor where
  getAbilities (LockedDoor a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage LockedDoor where
  runMessage msg t@(LockedDoor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        (LocationWithMostClues $ LocationWithoutTreachery $ treacheryIs Cards.lockedDoor)
        \locations -> chooseOrRunOne iid $ targetLabels locations $ only . Msg.attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let chooseSkillTest sType = SkillLabel sType [Msg.beginSkillTest sid iid (attrs.ability 1) attrs sType (Fixed 4)]
      chooseOne iid $ map chooseSkillTest [#combat, #agility]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LockedDoor <$> liftRunMessage msg attrs
