module Arkham.Types.Enemy.Cards.StubbornDetective
  ( StubbornDetective(..)
  , stubbornDetective
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Target

newtype StubbornDetective = StubbornDetective EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stubbornDetective :: EnemyCard StubbornDetective
stubbornDetective = enemyWith
  StubbornDetective
  Cards.stubbornDetective
  (3, Static 2, 2)
  (1, 0)
  (preyL .~ SetToBearer)

instance HasId LocationId env InvestigatorId => HasModifiersFor env StubbornDetective where
  getModifiersFor _ (InvestigatorTarget iid) (StubbornDetective a@EnemyAttrs {..})
    | spawned a
    = do
      locationId <- getId @LocationId iid
      pure $ toModifiers a [ Blank | locationId == enemyLocation ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env StubbornDetective where
  runMessage msg (StubbornDetective attrs) =
    StubbornDetective <$> runMessage msg attrs
