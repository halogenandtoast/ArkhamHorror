module Arkham.Asset.Cards.GuidingSpirit1
  ( guidingSpirit1
  , GuidingSpirit1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( AssetDefeated )
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype GuidingSpirit1 = GuidingSpirit1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidingSpirit1 :: AssetCard GuidingSpirit1
guidingSpirit1 = assetWith GuidingSpirit1 Cards.guidingSpirit1 (sanityL ?~ 3)

instance HasModifiersFor GuidingSpirit1 where
  getModifiersFor (AssetTarget aid) (GuidingSpirit1 attrs) | toId attrs == aid =
    pure $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor (InvestigatorTarget iid) (GuidingSpirit1 attrs) =
    pure $ toModifiers
      attrs
      [ SkillModifier SkillIntellect 1 | controlledBy attrs iid ]
  getModifiersFor _ _ = pure []

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
