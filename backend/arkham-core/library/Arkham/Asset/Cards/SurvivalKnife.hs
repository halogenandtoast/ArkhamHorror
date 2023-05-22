module Arkham.Asset.Cards.SurvivalKnife
  ( survivalKnife
  , SurvivalKnife(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Phase
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window ( WindowType )
import Arkham.Window qualified as Window

newtype SurvivalKnife = SurvivalKnife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalKnife :: AssetCard SurvivalKnife
survivalKnife = asset SurvivalKnife Cards.survivalKnife

instance HasAbilities SurvivalKnife where
  getAbilities (SurvivalKnife a) =
    [ restrictedAbility a 1 ControlsThis
      $ ActionAbility (Just Action.Fight) (ActionCost 1)
    , restrictedAbility a 2 (ControlsThis <> DuringPhase (PhaseIs EnemyPhase))
      $ ReactionAbility
          (DealtDamage Timing.After (SourceIsEnemyAttack AnyEnemy) You)
          (ExhaustCost $ toTarget a)
    ]

toEnemy :: [WindowType] -> EnemyId
toEnemy [] = error "called during incorrect window"
toEnemy (Window.DealtDamage (EnemyAttackSource eid) _ _ _ : _) = eid
toEnemy (_ : xs) = toEnemy xs

instance RunMessage SurvivalKnife where
  runMessage msg a@(SurvivalKnife attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [SkillModifier SkillCombat 1]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    UseCardAbility iid source 2 windows' _
      | isSource attrs source
      -> do
        pushAll
          [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [SkillModifier SkillCombat 2, DamageDealt 1]
          , FightEnemy iid (toEnemy $ map Window.windowType windows') source Nothing SkillCombat False
          ]
        pure a
    _ -> SurvivalKnife <$> runMessage msg attrs
