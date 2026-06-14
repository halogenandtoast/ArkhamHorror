module Arkham.Enemy.Cards.ColossalTyrant (colossalTyrant) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid (adjacentPositions)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ColossalTyrant = ColossalTyrant EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

colossalTyrant :: EnemyCard ColossalTyrant
colossalTyrant = enemy ColossalTyrant Cards.colossalTyrant (3, Static 6, 3) (2, 0)

instance HasModifiersFor ColossalTyrant where
  getModifiersFor (ColossalTyrant a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n, CannotMove]

instance HasAbilities ColossalTyrant where
  getAbilities (ColossalTyrant a) =
    extend1 a
      $ restricted a 1 (notExists $ investigatorAt (locationWithEnemy a))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage ColossalTyrant where
  runMessage msg e@(ColossalTyrant attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationWithEnemy attrs) >>= traverse_ \loc ->
        field LocationPosition loc >>= traverse_ \pos -> do
          adjacent <- select $ mapOneOf LocationInPosition (adjacentPositions pos)
          for_ adjacent \location -> do
            investigators <- select $ investigatorAt location
            for_ investigators \iid -> directHorror iid (attrs.ability 1) 1
            assets <- select $ assetAt location <> AssetWithSanity
            for_ assets \aid -> dealAssetDirectHorror aid (attrs.ability 1) 1
      pure e
    _ -> ColossalTyrant <$> liftRunMessage msg attrs
