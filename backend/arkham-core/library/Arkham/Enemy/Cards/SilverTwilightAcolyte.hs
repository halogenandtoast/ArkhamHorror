module Arkham.Enemy.Cards.SilverTwilightAcolyte (
  SilverTwilightAcolyte (..),
  silverTwilightAcolyte,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyAttacks)
import Arkham.Timing qualified as Timing

newtype SilverTwilightAcolyte = SilverTwilightAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightAcolyte :: EnemyCard SilverTwilightAcolyte
silverTwilightAcolyte =
  enemyWith
    SilverTwilightAcolyte
    Cards.silverTwilightAcolyte
    (2, Static 3, 3)
    (1, 0)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities SilverTwilightAcolyte where
  getAbilities (SilverTwilightAcolyte a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $
          ForcedAbility $
            EnemyAttacks Timing.After Anyone AnyEnemyAttack $
              EnemyWithId $
                toId a
      ]

instance RunMessage SilverTwilightAcolyte where
  runMessage msg e@(SilverTwilightAcolyte attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      e <$ push PlaceDoomOnAgenda
    _ -> SilverTwilightAcolyte <$> runMessage msg attrs
