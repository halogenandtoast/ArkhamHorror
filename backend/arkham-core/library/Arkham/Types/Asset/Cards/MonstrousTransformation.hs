module Arkham.Types.Asset.Cards.MonstrousTransformation
  ( MonstrousTransformation(..)
  , monstrousTransformation
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype MonstrousTransformation = MonstrousTransformation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousTransformation :: AssetCard MonstrousTransformation
monstrousTransformation = assetWith
  MonstrousTransformation
  Cards.monstrousTransformation
  (isStoryL .~ True)

instance HasModifiersFor env MonstrousTransformation where
  getModifiersFor _ (InvestigatorTarget iid) (MonstrousTransformation a)
    | ownedBy a iid = pure $ toModifiers
      a
      [ BaseSkillOf SkillWillpower 2
      , BaseSkillOf SkillIntellect 2
      , BaseSkillOf SkillCombat 5
      , BaseSkillOf SkillAgility 5
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions MonstrousTransformation where
  getActions (MonstrousTransformation a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ExhaustCost (toTarget a), ActionCost 1])
    ]

instance (AssetRunner env) => RunMessage env MonstrousTransformation where
  runMessage msg a@(MonstrousTransformation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> MonstrousTransformation <$> runMessage msg attrs
