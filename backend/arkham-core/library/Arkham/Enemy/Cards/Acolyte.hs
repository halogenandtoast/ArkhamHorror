module Arkham.Enemy.Cards.Acolyte (
  Acolyte (..),
  acolyte,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype Acolyte = Acolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acolyte :: EnemyCard Acolyte
acolyte =
  enemyWith
    Acolyte
    Cards.acolyte
    (3, Static 1, 2)
    (1, 0)
    (spawnAtL ?~ SpawnLocation EmptyLocation)

instance HasAbilities Acolyte where
  getAbilities (Acolyte a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 CanPlaceDoomOnThis
            $ ForcedAbility
            $ EnemySpawns Timing.After Anywhere
            $ EnemyWithId (toId a)
        ]

instance RunMessage Acolyte where
  runMessage msg e@(Acolyte attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceTokens (toSource attrs) (toTarget attrs) Doom 1
      pure e
    _ -> Acolyte <$> runMessage msg attrs
