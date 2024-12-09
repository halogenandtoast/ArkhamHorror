module Arkham.Treachery.Cards.SirenCall (sirenCall, SirenCall (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SirenCall = SirenCall TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sirenCall :: TreacheryCard SirenCall
sirenCall = treachery SirenCall Cards.sirenCall

instance HasModifiersFor SirenCall where
  getModifiersFor (SirenCall attrs) = case attrs.placement of
    InThreatArea iid -> do
      matchingIcons <- (#wild :) . toList <$> getSkillTestMatchingSkillIcons
      modifySelectMapM attrs (CardOwnedBy iid) \card -> do
        let n = count (`elem` matchingIcons) (cdSkills $ toCardDef card)
        pure [AdditionalCostToCommit iid $ ResourceCost n | n > 0]
    _ -> pure mempty

instance HasAbilities SirenCall where
  getAbilities (SirenCall a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage SirenCall where
  runMessage msg t@(SirenCall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SirenCall <$> liftRunMessage msg attrs
