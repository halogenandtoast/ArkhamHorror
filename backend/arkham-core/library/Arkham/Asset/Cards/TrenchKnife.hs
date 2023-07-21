module Arkham.Asset.Cards.TrenchKnife (
  trenchKnife,
  TrenchKnife (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype TrenchKnife = TrenchKnife AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchKnife :: AssetCard TrenchKnife
trenchKnife = asset TrenchKnife Cards.trenchKnife

instance HasModifiersFor TrenchKnife where
  getModifiersFor (InvestigatorTarget iid) (TrenchKnife attrs)
    | attrs `controlledBy` iid =
        pure $
          toModifiers
            attrs
            [ActionDoesNotCauseAttacksOfOpportunity Action.Engage]
  getModifiersFor _ _ = pure []

instance HasAbilities TrenchKnife where
  getAbilities (TrenchKnife attrs) =
    [ restrictedAbility attrs 1 ControlsThis $
        ActionAbility (Just Action.Fight) $
          ActionCost 1
    ]

instance RunMessage TrenchKnife where
  runMessage msg a@(TrenchKnife attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      enemyCount <- selectCount EnemyEngagedWithYou
      a
        <$ pushAll
          [ skillTestModifier
              attrs
              (InvestigatorTarget iid)
              (SkillModifier SkillCombat enemyCount)
          , ChooseFightEnemy iid source Nothing SkillCombat mempty False
          ]
    _ -> TrenchKnife <$> runMessage msg attrs
