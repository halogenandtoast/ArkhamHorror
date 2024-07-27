module Arkham.Asset.Cards.MindsEye2 (
  mindsEye2,
  MindsEye2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype MindsEye2 = MindsEye2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindsEye2 :: AssetCard MindsEye2
mindsEye2 = asset MindsEye2 Cards.mindsEye2

-- TODO: This window should be better
instance HasAbilities MindsEye2 where
  getAbilities (MindsEye2 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          ( InitiatedSkillTest #when You (SkillTypeOneOf [#intellect, #combat, #agility]) AnySkillTestValue #any
          )
          (assetUseCost attrs Secret 1)
    , restrictedAbility attrs 2 ControlsThis
        $ FastAbility
        $ HandDiscardCost 1
        $ basic
        $ cardIs Cards.mindsEye2
    ]

instance RunMessage MindsEye2 where
  runMessage msg a@(MindsEye2 attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pushAll
        [ ReplaceSkillTestSkill (FromSkillType sType) (ToSkillType #willpower)
        | sType <- [#intellect, #combat, #agility]
        ]
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      push $ AddUses (attrs.ability 2) (toId a) Secret 2
      pure a
    _ -> MindsEye2 <$> runMessage msg attrs
