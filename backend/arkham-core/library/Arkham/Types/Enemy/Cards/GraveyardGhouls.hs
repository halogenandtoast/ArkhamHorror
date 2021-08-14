module Arkham.Types.Enemy.Cards.GraveyardGhouls
  ( graveyardGhouls
  , GraveyardGhouls(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Target

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

instance HasModifiersFor env GraveyardGhouls where
  getModifiersFor _ (InvestigatorTarget iid) (GraveyardGhouls attrs)
    | iid `elem` enemyEngagedInvestigators attrs = pure
    $ toModifiers attrs [CardsCannotLeaveYourDiscardPile]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasAbilities env => HasAbilities env GraveyardGhouls where
  getAbilities i window (GraveyardGhouls attrs) = getAbilities i window attrs

instance EnemyAttrsRunMessage env => RunMessage env GraveyardGhouls where
  runMessage msg (GraveyardGhouls attrs) =
    GraveyardGhouls <$> runMessage msg attrs
