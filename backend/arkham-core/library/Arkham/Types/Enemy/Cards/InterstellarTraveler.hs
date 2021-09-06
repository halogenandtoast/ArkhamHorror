module Arkham.Types.Enemy.Cards.InterstellarTraveler
  ( interstellarTraveler
  , InterstellarTraveler(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

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

instance EnemyRunner env => RunMessage env InterstellarTraveler where
  runMessage msg e@(InterstellarTraveler attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      let lid = enemyLocation attrs
      clueCount <- unClueCount <$> getCount lid
      push (PlaceDoom (toTarget attrs) 1)
      e <$ when (clueCount > 0) (push $ RemoveClues (LocationTarget lid) 1)
    _ -> InterstellarTraveler <$> runMessage msg attrs
