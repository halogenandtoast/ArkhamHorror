module Arkham.Enemy.Cards.TommyMalloy (
  tommyMalloy,
  tommyMalloyEffect,
  TommyMalloy (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TommyMalloy = TommyMalloy EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

tommyMalloy :: EnemyCard TommyMalloy
tommyMalloy =
  enemyWith
    TommyMalloy
    Cards.tommyMalloy
    (2, Static 3, 3)
    (2, 0)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities TommyMalloy where
  getAbilities (TommyMalloy attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyTakeDamage #when AnyDamageEffect (EnemyWithId $ toId attrs) AnyValue AnySource
      ]

instance RunMessage TommyMalloy where
  runMessage msg e@(TommyMalloy attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ createCardEffect Cards.tommyMalloy Nothing (attrs.ability 1) attrs
      pure e
    _ -> TommyMalloy <$> runMessage msg attrs

newtype TommyMalloyEffect = TommyMalloyEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

tommyMalloyEffect :: EffectArgs -> TommyMalloyEffect
tommyMalloyEffect = cardEffect TommyMalloyEffect Cards.tommyMalloy

instance HasModifiersFor TommyMalloyEffect where
  getModifiersFor target (TommyMalloyEffect attrs) | attrs.target == target = do
    pure $ toModifiers attrs [MaxDamageTaken 1]
  getModifiersFor _ _ = pure []

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case attrs.target of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') _ ->
      eid == eid' && window.timing == #after
    _ -> False

instance RunMessage TommyMalloyEffect where
  runMessage msg e@(TommyMalloyEffect attrs) = case msg of
    CheckWindow _ windows' | any (isTakeDamage attrs) windows' -> do
      push $ disable attrs
      pure e
    _ -> TommyMalloyEffect <$> runMessage msg attrs
