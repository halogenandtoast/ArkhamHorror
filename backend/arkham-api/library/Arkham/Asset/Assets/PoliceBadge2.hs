module Arkham.Asset.Assets.PoliceBadge2 (policeBadge2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype PoliceBadge2 = PoliceBadge2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

policeBadge2 :: AssetCard PoliceBadge2
policeBadge2 = asset PoliceBadge2 Cards.policeBadge2

instance HasModifiersFor PoliceBadge2 where
  getModifiersFor (PoliceBadge2 a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities PoliceBadge2 where
  getAbilities (PoliceBadge2 a) = [controlled a 1 criteria $ FastAbility $ DiscardCost FromPlay (toTarget a)]
   where
    criteria = exists (affectsOthers $ TurnInvestigator <> colocatedWithMatch You)

instance RunMessage PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach TurnInvestigator \iid -> gainActions iid (attrs.ability 1) 2
      pure a
    _ -> PoliceBadge2 <$> liftRunMessage msg attrs
