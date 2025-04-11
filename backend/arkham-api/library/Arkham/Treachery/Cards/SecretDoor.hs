module Arkham.Treachery.Cards.SecretDoor (secretDoor) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretDoor = SecretDoor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretDoor :: TreacheryCard SecretDoor
secretDoor = treachery SecretDoor Cards.secretDoor

instance HasModifiersFor SecretDoor where
  getModifiersFor (SecretDoor attrs) = case attrs.placement of
    AttachedToLocation lid -> modified_ attrs lid [AdditionalCostToLeave UnpayableCost]
    _ -> pure mempty

instance HasAbilities SecretDoor where
  getAbilities (SecretDoor a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage SecretDoor where
  runMessage msg t@(SecretDoor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        (LocationWithMostInvestigators $ LocationWithoutTreachery $ treacheryIs Cards.secretDoor)
        \locations -> chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind -> do
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) attrs kind (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SecretDoor <$> liftRunMessage msg attrs
