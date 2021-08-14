module Arkham.Types.Enemy.Cards.CarnevaleSentinel
  ( carnevaleSentinel
  , CarnevaleSentinel(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Target

newtype CarnevaleSentinel = CarnevaleSentinel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

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

instance EnemyAttrsRunMessage env => RunMessage env CarnevaleSentinel where
  runMessage msg (CarnevaleSentinel attrs) = case msg of
    InvestigatorDrawEnemy _ lid eid | eid == toId attrs -> do
      acrossLocationId <- getAcrossLocation lid
      CarnevaleSentinel <$> runMessage
        msg
        (attrs & spawnAtL ?~ LocationWithId acrossLocationId)
    _ -> CarnevaleSentinel <$> runMessage msg attrs
