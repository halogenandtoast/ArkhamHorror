module Arkham.Enemy.Cards.TidalTerror (
  tidalTerror,
  TidalTerror (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype TidalTerror = TidalTerror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tidalTerror :: EnemyCard TidalTerror
tidalTerror =
  enemyWith
    TidalTerror
    Cards.tidalTerror
    (4, Static 4, 2)
    (1, 2)
    ( spawnAtL
        ?~ SpawnAt
          ( LocationMatchAny
              [ LocationWithTitle "Porte de l'Avancée"
              , LocationWithTitle "Chapel of St. Aubert"
              ]
          )
    )

instance RunMessage TidalTerror where
  runMessage msg (TidalTerror attrs) = TidalTerror <$> runMessage msg attrs
