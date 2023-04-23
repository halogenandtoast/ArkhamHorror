module Arkham.Enemy.Cards.HarbingerOfValusia
  ( harbingerOfValusia
  , HarbingerOfValusia(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype HarbingerOfValusia = HarbingerOfValusia EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harbingerOfValusia :: EnemyCard HarbingerOfValusia
harbingerOfValusia =
  enemy HarbingerOfValusia Cards.harbingerOfValusia (3, PerPlayer 10, 3) (2, 2)

instance HasModifiersFor HarbingerOfValusia where
  getModifiersFor target (HarbingerOfValusia a) | isTarget a target =
    pure $ toModifiers a [CanRetaliateWhileExhausted]
  getModifiersFor _ _ = pure []

instance HasAbilities HarbingerOfValusia where
  getAbilities (HarbingerOfValusia a) = withBaseAbilities
    a
    [ limitedAbility (PlayerLimit PerTestOrAbility 1)
      $ mkAbility a 1
      $ ForcedAbility
      $ OrWindowMatcher
          [ SkillTestResult
            Timing.After
            You
            (WhileEvadingAnEnemy $ EnemyWithId $ toId a)
            (SuccessResult AnyValue)
          , SkillTestResult
            Timing.After
            You
            (WhileAttackingAnEnemy $ EnemyWithId $ toId a)
            (SuccessResult AnyValue)
          ]
    ]

instance RunMessage HarbingerOfValusia where
  runMessage msg e@(HarbingerOfValusia attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      n <- getPlayerCountValue (PerPlayer 2)
      if enemyResources attrs + 1 >= n
        then do
          let
            damage = enemyDamage attrs
            enemy' = overAttrs (damageL .~ damage)
              $ cbCardBuilder harbingerOfValusia (toCardId attrs) (toId attrs)
          push $ SetOutOfPlay SetAsideZone (toTarget attrs)
          pure enemy'
        else do
          push $ PlaceResources (toTarget attrs) 1
          pure e
    _ -> HarbingerOfValusia <$> runMessage msg attrs
