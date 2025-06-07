module Arkham.Treachery.Cards.Overgrowth (overgrowth) where

import Arkham.Ability
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Overgrowth = Overgrowth TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrowth :: TreacheryCard Overgrowth
overgrowth = treachery Overgrowth Cards.overgrowth

instance HasModifiersFor Overgrowth where
  getModifiersFor (Overgrowth attrs) = case attrs.placement of
    AttachedToLocation lid -> modifySelect attrs (investigatorAt lid) [CannotExplore]
    _ -> pure mempty

instance HasAbilities Overgrowth where
  getAbilities (Overgrowth a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage Overgrowth where
  runMessage msg t@(Overgrowth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        whenMatch lid (LocationWithoutTreachery $ treacheryIs Cards.overgrowth) $ attachTreachery attrs lid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #intellect] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) attrs sType (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Overgrowth <$> liftRunMessage msg attrs
