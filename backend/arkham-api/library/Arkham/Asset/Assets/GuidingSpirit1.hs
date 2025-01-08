module Arkham.Asset.Assets.GuidingSpirit1 (
  guidingSpirit1,
  GuidingSpirit1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (AssetDefeated)
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype GuidingSpirit1 = GuidingSpirit1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidingSpirit1 :: AssetCard GuidingSpirit1
guidingSpirit1 = assetWith GuidingSpirit1 Cards.guidingSpirit1 (sanityL ?~ 3)

instance HasModifiersFor GuidingSpirit1 where
  getModifiersFor (GuidingSpirit1 a) = do
    self <- modifySelf a [NonDirectHorrorMustBeAssignToThisFirst]
    controller <- controllerGets a [SkillModifier #intellect 1]
    pure $ self <> controller

instance HasAbilities GuidingSpirit1 where
  getAbilities (GuidingSpirit1 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ForcedAbility
        $ AssetDefeated Timing.When ByHorror
        $ AssetWithId
        $ toId a
    ]

instance RunMessage GuidingSpirit1 where
  runMessage msg a@(GuidingSpirit1 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Exile (toTarget attrs)
      pure a
    _ -> GuidingSpirit1 <$> runMessage msg attrs
