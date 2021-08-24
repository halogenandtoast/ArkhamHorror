module Arkham.Types.Enemy.Cards.RuthTurner
  ( ruthTurner
  , RuthTurner(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyEvaded)
import qualified Arkham.Types.Timing as Timing

newtype RuthTurner = RuthTurner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthTurner :: EnemyCard RuthTurner
ruthTurner = enemyWith
  RuthTurner
  Cards.ruthTurner
  (2, Static 4, 5)
  (1, 0)
  (spawnAtL ?~ LocationWithTitle "St. Mary's Hospital")

instance HasAbilities env RuthTurner where
  getAbilities iid window (RuthTurner a) =
    withBaseAbilities iid window a $ pure
      [ mkAbility a 1
        $ ForcedAbility
        $ EnemyEvaded Timing.After Anyone
        $ EnemyWithId
        $ toId a
      ]

instance EnemyRunner env => RunMessage env RuthTurner where
  runMessage msg e@(RuthTurner attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (AddToVictory $ toTarget attrs)
    _ -> RuthTurner <$> runMessage msg attrs
