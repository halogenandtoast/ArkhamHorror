module Arkham.Asset.Assets.GuidingSpirit1 (guidingSpirit1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (AssetDefeated)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype GuidingSpirit1 = GuidingSpirit1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidingSpirit1 :: AssetCard GuidingSpirit1
guidingSpirit1 = assetWith GuidingSpirit1 Cards.guidingSpirit1 (sanityL ?~ 3)

instance HasModifiersFor GuidingSpirit1 where
  getModifiersFor (GuidingSpirit1 a) = do
    modifySelf a [NonDirectHorrorMustBeAssignToThisFirst]
    controllerGets a [SkillModifier #intellect 1]

instance HasAbilities GuidingSpirit1 where
  getAbilities (GuidingSpirit1 a) =
    [restricted a 1 ControlsThis $ forced $ AssetDefeated #when ByHorror (be a)]

instance RunMessage GuidingSpirit1 where
  runMessage msg a@(GuidingSpirit1 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Exile (toTarget attrs)
      pure a
    _ -> GuidingSpirit1 <$> runMessage msg attrs
