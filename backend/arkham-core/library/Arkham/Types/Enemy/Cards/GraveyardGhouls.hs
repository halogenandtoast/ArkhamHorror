module Arkham.Types.Enemy.Cards.GraveyardGhouls
  ( graveyardGhouls
  , GraveyardGhouls(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Prey

newtype GraveyardGhouls = GraveyardGhouls EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyardGhouls :: EnemyCard GraveyardGhouls
graveyardGhouls = enemyWith
  GraveyardGhouls
  Cards.graveyardGhouls
  (3, Static 3, 2)
  (1, 1)
  (preyL .~ SetToBearer)

instance HasModifiersFor env GraveyardGhouls

instance EnemyAttrsHasActions env => HasActions env GraveyardGhouls where
  getActions i window (GraveyardGhouls attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env GraveyardGhouls where
  runMessage msg (GraveyardGhouls attrs) =
    GraveyardGhouls <$> runMessage msg attrs
