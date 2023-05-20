module Arkham.Enemy.Cards.InterstellarTraveler (
  interstellarTraveler,
  InterstellarTraveler (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype InterstellarTraveler = InterstellarTraveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interstellarTraveler :: EnemyCard InterstellarTraveler
interstellarTraveler =
  enemyWith
    InterstellarTraveler
    Cards.interstellarTraveler
    (4, Static 3, 2)
    (1, 2)
    (spawnAtL ?~ SpawnLocation (LocationWithTrait Extradimensional))

instance HasAbilities InterstellarTraveler where
  getAbilities (InterstellarTraveler attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $
          ForcedAbility $
            EnemyEnters Timing.When Anywhere $
              EnemyWithId $
                toId attrs
      ]

instance RunMessage InterstellarTraveler where
  runMessage msg e@(InterstellarTraveler attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      enemyLocation <- field EnemyLocation (toId attrs)
      for_ enemyLocation $ \loc -> do
        clueCount <- field LocationClues loc
        push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
        when (clueCount > 0) (push $ RemoveClues (toAbilitySource attrs 1) (toTarget loc) 1)
      pure e
    _ -> InterstellarTraveler <$> runMessage msg attrs
