module Arkham.Enemy.Cards.CarnevaleSentinel
  ( carnevaleSentinel
  , CarnevaleSentinel(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Name
import Arkham.Target

newtype CarnevaleSentinel = CarnevaleSentinel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- TODO: Should use spawnAtL for this
carnevaleSentinel :: EnemyCard CarnevaleSentinel
carnevaleSentinel =
  enemy CarnevaleSentinel Cards.carnevaleSentinel (3, Static 3, 3) (2, 0)

instance (HasName env AssetId, HasId (Maybe LocationId) env AssetId) => HasModifiersFor env CarnevaleSentinel where
  getModifiersFor _ (AssetTarget aid) (CarnevaleSentinel attrs) = do
    mlid <- getId @(Maybe LocationId) aid
    case mlid of
      Just lid | lid == enemyLocation attrs -> do
        name <- getName aid
        pure $ toModifiers
          attrs
          [ CannotBeRevealed | nameTitle name == "Masked Carnevale-Goer" ]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env CarnevaleSentinel where
  runMessage msg (CarnevaleSentinel attrs) = case msg of
    InvestigatorDrawEnemy _ lid eid | eid == toId attrs -> do
      acrossLocationId <- getAcrossLocation lid
      CarnevaleSentinel <$> runMessage
        msg
        (attrs & spawnAtL ?~ LocationWithId acrossLocationId)
    _ -> CarnevaleSentinel <$> runMessage msg attrs
