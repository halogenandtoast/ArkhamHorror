module Arkham.Treachery.Cards.CaughtInAWeb (caughtInAWeb, CaughtInAWeb (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaughtInAWeb = CaughtInAWeb TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtInAWeb :: TreacheryCard CaughtInAWeb
caughtInAWeb = treachery CaughtInAWeb Cards.caughtInAWeb

instance HasModifiersFor CaughtInAWeb where
  getModifiersFor (CaughtInAWeb a) = case a.placement of
    InThreatArea iid -> do
      alreadyMoved <- fieldMap InvestigatorActionsTaken (any (elem #move)) iid
      modified_ a iid $ SkillModifier #agility (-1) : [CannotTakeAction #move | alreadyMoved]
    _ -> pure mempty

instance HasAbilities CaughtInAWeb where
  getAbilities (CaughtInAWeb attrs) = [skillTestAbility $ restrictedAbility attrs 1 OnSameLocation actionAbility]

instance RunMessage CaughtInAWeb where
  runMessage msg t@(CaughtInAWeb attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CaughtInAWeb <$> liftRunMessage msg attrs
