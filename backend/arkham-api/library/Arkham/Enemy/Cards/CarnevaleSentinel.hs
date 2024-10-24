module Arkham.Enemy.Cards.CarnevaleSentinel (carnevaleSentinel, CarnevaleSentinel (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers

newtype CarnevaleSentinel = CarnevaleSentinel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- TODO: Should use spawnAtL for this
carnevaleSentinel :: EnemyCard CarnevaleSentinel
carnevaleSentinel = enemy CarnevaleSentinel Cards.carnevaleSentinel (3, Static 3, 3) (2, 0)

instance HasModifiersFor CarnevaleSentinel where
  getModifiersFor (AssetTarget aid) (CarnevaleSentinel attrs) = do
    mlid <- field AssetLocation aid
    enemyLocation <- field EnemyLocation (toId attrs)
    case mlid of
      Just lid | Just lid == enemyLocation -> do
        name <- field AssetName aid
        toModifiers attrs [CannotBeRevealed | nameTitle name == "Masked Carnevale-Goer"]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage CarnevaleSentinel where
  runMessage msg (CarnevaleSentinel attrs) = case msg of
    InvestigatorDrawEnemy iid eid | eid == toId attrs -> do
      mAcrossLocationId <- maybe (pure Nothing) getAcrossLocation =<< getMaybeLocation iid

      CarnevaleSentinel
        <$> runMessage msg (attrs & spawnAtL ?~ SpawnAt (maybe Nowhere LocationWithId mAcrossLocationId))
    _ -> CarnevaleSentinel <$> runMessage msg attrs
