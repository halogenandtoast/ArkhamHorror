module Arkham.Asset.Cards.GuardDog (GuardDog (..), guardDog) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GuardDog = GuardDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

guardDog :: AssetCard GuardDog
guardDog = ally GuardDog Cards.guardDog (3, 1)

instance HasAbilities GuardDog where
  getAbilities (GuardDog x) =
    [ controlledAbility x 1 CanDealDamage
        $ freeReaction
        $ AssetDealtDamage #when (SourceIsEnemyAttack AnyEnemy) (AssetWithId (toId x))
    ]

toEnemyId :: [Window] -> EnemyId
toEnemyId [] = error "invalid"
toEnemyId ((windowType -> Window.DealtDamage source _ _ _) : ws) = case source of
  EnemySource eid -> eid
  EnemyAttackSource eid -> eid
  _ -> toEnemyId ws
toEnemyId (_ : ws) = toEnemyId ws

instance RunMessage GuardDog where
  runMessage msg a@(GuardDog attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (toEnemyId -> eid) _ -> do
      push $ nonAttackEnemyDamage (attrs.ability 1) 1 eid
      pure a
    _ -> GuardDog <$> runMessage msg attrs
