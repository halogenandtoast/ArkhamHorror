module Arkham.Asset.Cards.PoliceBadge2 (PoliceBadge2 (..), policeBadge2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype PoliceBadge2 = PoliceBadge2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

policeBadge2 :: AssetCard PoliceBadge2
policeBadge2 = asset PoliceBadge2 Cards.policeBadge2

instance HasModifiersFor PoliceBadge2 where
  getModifiersFor (InvestigatorTarget iid) (PoliceBadge2 a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #willpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities PoliceBadge2 where
  getAbilities (PoliceBadge2 a) = [controlledAbility a 1 criteria $ FastAbility $ DiscardCost FromPlay (toTarget a)]
   where
    criteria = exists (affectsOthers $ TurnInvestigator <> at_ YourLocation)

instance RunMessage PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iid <- selectJust TurnInvestigator
      push $ GainActions iid (attrs.ability 1) 2
      pure a
    _ -> PoliceBadge2 <$> runMessage msg attrs
