module Arkham.Asset.Assets.AceOfSwords1 (aceOfSwords1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype AceOfSwords1 = AceOfSwords1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceOfSwords1 :: AssetCard AceOfSwords1
aceOfSwords1 = asset AceOfSwords1 Cards.aceOfSwords1

instance HasModifiersFor AceOfSwords1 where
  getModifiersFor (AceOfSwords1 a) = for_ a.controller \iid -> do
    modified_ a iid [SkillModifier #combat 1]

instance HasAbilities AceOfSwords1 where
  getAbilities (AceOfSwords1 a) = [reactionAbility a 1 Free (GameBegins #when) InYourHand]

instance RunMessage AceOfSwords1 where
  runMessage msg a@(AceOfSwords1 attrs) = runQueueT $ case msg of
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 1) | iid == iid' -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> AceOfSwords1 <$> liftRunMessage msg attrs
