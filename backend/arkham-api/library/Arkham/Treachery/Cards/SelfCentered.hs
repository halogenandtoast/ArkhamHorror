module Arkham.Treachery.Cards.SelfCentered (selfCentered, SelfCentered (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SelfCentered = SelfCentered TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selfCentered :: TreacheryCard SelfCentered
selfCentered = treachery SelfCentered Cards.selfCentered

instance HasModifiersFor SelfCentered where
  getModifiersFor (InvestigatorTarget iid) (SelfCentered attrs) | treacheryInThreatArea iid attrs = do
    modified
      attrs
      [CannotCommitToOtherInvestigatorsSkillTests, CannotAffectOtherPlayersWithPlayerEffectsExceptDamage]
  getModifiersFor _ _ = pure []

instance HasAbilities SelfCentered where
  getAbilities (SelfCentered a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage SelfCentered where
  runMessage msg t@(SelfCentered attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SelfCentered <$> liftRunMessage msg attrs
