module Arkham.Asset.Cards.GuardDog2
  ( GuardDog2(..)
  , guardDog2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Id
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype GuardDog2 = GuardDog2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog2 :: AssetCard GuardDog2
guardDog2 = ally GuardDog2 Cards.guardDog2 (4, 2)

instance HasAbilities GuardDog2 where
  getAbilities (GuardDog2 x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> EnemyCriteria
          (EnemyExists $ EnemyAt YourLocation <> CanEngageEnemy)
        )
      $ FastAbility
      $ ExhaustCost
      $ toTarget x
    , restrictedAbility x 2 ControlsThis $ ReactionAbility
      (AssetDealtDamage
        Timing.When
        (SourceIsEnemyAttack AnyEnemy)
        (AssetWithId (toId x))
      )
      Free
    ]

toEnemyId :: [Window] -> EnemyId
toEnemyId [] = error "invalid"
toEnemyId (Window _ (Window.DealtDamage source _ _ _) : ws) = case source of
  EnemySource eid -> eid
  EnemyAttackSource eid -> eid
  _ -> toEnemyId ws
toEnemyId (_ : ws) = toEnemyId ws

instance RunMessage GuardDog2 where
  runMessage msg a@(GuardDog2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      enemies <-
        selectList $ EnemyAt (locationWithInvestigator iid) <> CanEngageEnemy
      push $ chooseOrRunOne
        iid
        [ targetLabel
            enemy
            [ EngageEnemy iid enemy False
            , InitiateEnemyAttack $ enemyAttack enemy iid
            ]
        | enemy <- enemies
        ]
      pure a
    UseCardAbility _ source 2 (toEnemyId -> eid) _ | isSource attrs source -> do
      push $ EnemyDamage eid $ nonAttack source 1
      pure a
    _ -> GuardDog2 <$> runMessage msg attrs
