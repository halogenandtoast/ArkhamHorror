module Arkham.Asset.Cards.PoliceBadge2 (
  PoliceBadge2 (..),
  policeBadge2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype PoliceBadge2 = PoliceBadge2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

policeBadge2 :: AssetCard PoliceBadge2
policeBadge2 = asset PoliceBadge2 Cards.policeBadge2

instance HasModifiersFor PoliceBadge2 where
  getModifiersFor (InvestigatorTarget iid) (PoliceBadge2 a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #willpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities PoliceBadge2 where
  getAbilities (PoliceBadge2 a) = [restrictedAbility a 1 criteria $ FastAbility $ DiscardCost FromPlay (toTarget a)]
   where
    criteria = ControlsThis <> InvestigatorExists (TurnInvestigator <> InvestigatorAt YourLocation)

instance RunMessage PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iid <- selectJust TurnInvestigator
      push $ GainActions iid (toAbilitySource attrs 1) 2
      pure a
    _ -> PoliceBadge2 <$> runMessage msg attrs
