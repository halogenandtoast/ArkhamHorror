module Arkham.Treachery.Cards.HastyRepairs (hastyRepairs, HastyRepairs (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HastyRepairs = HastyRepairs TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastyRepairs :: TreacheryCard HastyRepairs
hastyRepairs = treachery HastyRepairs Cards.hastyRepairs

instance HasModifiersFor HastyRepairs where
  getModifiersFor (InvestigatorTarget iid) (HastyRepairs attrs) = do
    maybeModified attrs do
      guard $ treacheryInThreatArea iid attrs
      source <- MaybeT getSkillTestSource
      asset <- hoistMaybe source.asset
      liftGuardM $ asset <=~> assetControlledBy iid
      pure [BaseSkill 0]
  getModifiersFor _ _ = pure []

instance HasAbilities HastyRepairs where
  getAbilities (HastyRepairs a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage HastyRepairs where
  runMessage msg t@(HastyRepairs attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> HastyRepairs <$> liftRunMessage msg attrs
