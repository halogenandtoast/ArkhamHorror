module Arkham.Asset.Cards.TrenchKnife
  ( trenchKnife
  , TrenchKnife(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype TrenchKnife = TrenchKnife AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchKnife :: AssetCard TrenchKnife
trenchKnife = asset TrenchKnife Cards.trenchKnife

instance HasModifiersFor env TrenchKnife where
  getModifiersFor _ (InvestigatorTarget iid) (TrenchKnife attrs)
    | attrs `ownedBy` iid = pure $ toModifiers
      attrs
      [ActionDoesNotCauseAttacksOfOpportunity Action.Engage]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TrenchKnife where
  getAbilities (TrenchKnife attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance AssetRunner env => RunMessage env TrenchKnife where
  runMessage msg a@(TrenchKnife attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemyCount <- selectCount EnemyEngagedWithYou
      a <$ pushAll
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat enemyCount)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> TrenchKnife <$> runMessage msg attrs
