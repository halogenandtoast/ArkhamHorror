module Arkham.Treachery.Cards.SirenCall (sirenCall, SirenCall (..)) where

import Arkham.Ability
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SirenCall = SirenCall TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sirenCall :: TreacheryCard SirenCall
sirenCall = treachery SirenCall Cards.sirenCall

instance HasModifiersFor SirenCall where
  getModifiersFor (CardIdTarget cid) (SirenCall attrs) = do
    withTreacheryInvestigator attrs $ \iid -> do
      matchingIcons <- (#wild :) . toList <$> getSkillTestMatchingSkillIcons
      n <- count (`elem` matchingIcons) . cdSkills . toCardDef <$> getCard cid
      pure $ toModifiers attrs [AdditionalCostToCommit iid $ ResourceCost n]
  getModifiersFor _ _ = pure []

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
