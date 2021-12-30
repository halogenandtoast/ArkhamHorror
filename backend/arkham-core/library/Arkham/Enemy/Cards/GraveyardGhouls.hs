module Arkham.Enemy.Cards.GraveyardGhouls
  ( graveyardGhouls
  , GraveyardGhouls(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Modifier
import Arkham.Prey
import Arkham.Target

newtype GraveyardGhouls = GraveyardGhouls EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

graveyardGhouls :: EnemyCard GraveyardGhouls
graveyardGhouls = enemyWith
  GraveyardGhouls
  Cards.graveyardGhouls
  (3, Static 3, 2)
  (1, 1)
  (preyL .~ SetToBearer)

instance HasModifiersFor env GraveyardGhouls where
  getModifiersFor _ (InvestigatorTarget iid) (GraveyardGhouls attrs)
    | iid `elem` enemyEngagedInvestigators attrs = pure
    $ toModifiers attrs [CardsCannotLeaveYourDiscardPile]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env GraveyardGhouls where
  runMessage msg (GraveyardGhouls attrs) =
    GraveyardGhouls <$> runMessage msg attrs
