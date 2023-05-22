module Arkham.Asset.Cards.GuardDog
  ( GuardDog(..)
  , guardDog
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Id
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype GuardDog = GuardDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog :: AssetCard GuardDog
guardDog = ally GuardDog Cards.guardDog (3, 1)

instance HasAbilities GuardDog where
  getAbilities (GuardDog x) =
    [ restrictedAbility x 1 ControlsThis $ ReactionAbility
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

instance RunMessage GuardDog where
  runMessage msg a@(GuardDog attrs) = case msg of
    UseCardAbility _ source 1 (toEnemyId -> eid) _ | isSource attrs source -> do
      push $ EnemyDamage eid $ nonAttack source 1
      pure a
    _ -> GuardDog <$> runMessage msg attrs
