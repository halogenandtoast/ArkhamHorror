module Arkham.Enemy.Cards.Fanatic (fanatic) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype Fanatic = Fanatic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fanatic :: EnemyCard Fanatic
fanatic =
  enemy Fanatic Cards.fanatic (3, Static 2, 3) (1, 0)
    & setSpawnAt (LocationWithMostClues RevealedLocation)

instance HasAbilities Fanatic where
  getAbilities (Fanatic a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #after LocationWithAnyClues (be a)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a <> EnemyWithAnyClues)
      ]

instance RunMessage Fanatic where
  runMessage msg e@(Fanatic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> moveTokens (attrs.ability 1) loc attrs #clue 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      moveAllTokens (attrs.ability 2) attrs iid #clue
      pure e
    _ -> Fanatic <$> liftRunMessage msg attrs
