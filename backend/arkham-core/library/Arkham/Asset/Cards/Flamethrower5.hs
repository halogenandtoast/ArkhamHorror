module Arkham.Asset.Cards.Flamethrower5
  ( flamethrower5
  , Flamethrower5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Projection
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Enemy.Types qualified as Field ( Field (..) )

newtype Flamethrower5 = Flamethrower5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flamethrower5 :: AssetCard Flamethrower5
flamethrower5 = asset Flamethrower5 Cards.flamethrower5

instance HasAbilities Flamethrower5 where
  getAbilities (Flamethrower5 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage Flamethrower5 where
  runMessage msg a@(Flamethrower5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemies <- withMaxField Field.EnemyFight =<< selectList (enemyEngagedWith iid)
      pushAll
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat 4)
        , ChooseFightEnemy
          iid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillCombat
          (EnemyOneOf $ map EnemyWithId enemies)
          False
        ]
      pure a
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      -- damage <- damageValueFor 4 iid
      -- engaged <- selectList $ enemyEngagedWith iid
      -- let
      --   toMsg eid' = if eid == eid'
      --     then EnemyDamage eid' iid (toSource attrs) AttackDamageEffect 2
      --     else DirectEnemyDamage
      --       eid'
      --       iid
      --       (toSource attrs)
      --       AttackDamageEffect
      --       2
      -- msgs <- selectListMap toMsg $ EnemyAt YourLocation
      -- e <$ pushAll msgs
      pure a
    _ -> Flamethrower5 <$> runMessage msg attrs
