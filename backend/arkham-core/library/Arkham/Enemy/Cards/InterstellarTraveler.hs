module Arkham.Enemy.Cards.InterstellarTraveler
  ( interstellarTraveler
  , InterstellarTraveler(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Query
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype InterstellarTraveler = InterstellarTraveler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interstellarTraveler :: EnemyCard InterstellarTraveler
interstellarTraveler = enemyWith
  InterstellarTraveler
  Cards.interstellarTraveler
  (4, Static 3, 2)
  (1, 2)
  (spawnAtL ?~ LocationWithTrait Extradimensional)

instance HasAbilities InterstellarTraveler where
  getAbilities (InterstellarTraveler attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ EnemyEnters Timing.When Anywhere
      $ EnemyWithId
      $ toId attrs
    ]

instance EnemyRunner env => RunMessage InterstellarTraveler where
  runMessage msg e@(InterstellarTraveler attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> case enemyLocation attrs of
      Nothing -> pure e
      Just loc -> do
        clueCount <- unClueCount <$> getCount loc
        push (PlaceDoom (toTarget attrs) 1)
        e <$ when (clueCount > 0) (push $ RemoveClues (LocationTarget loc) 1)
    _ -> InterstellarTraveler <$> runMessage msg attrs
