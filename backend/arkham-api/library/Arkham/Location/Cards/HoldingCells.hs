module Arkham.Location.Cards.HoldingCells (holdingCells, HoldingCells (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype HoldingCells = HoldingCells LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holdingCells :: LocationCard HoldingCells
holdingCells = location HoldingCells Cards.holdingCells 3 (PerPlayer 1)

instance HasModifiersFor HoldingCells where
  getModifiersFor target (HoldingCells a) | a `is` target = do
    toModifiers a [CannotBeEnteredBy AnyEnemy]
  getModifiersFor (EnemyTarget _) (HoldingCells a) = do
    toModifiers a [ChangeSpawnLocation (be a) (LocationWithTitle "Sunken Grotto")]
  getModifiersFor _ _ = pure []

instance HasAbilities HoldingCells where
  getAbilities (HoldingCells attrs) =
    extendRevealed attrs []

instance RunMessage HoldingCells where
  runMessage msg (HoldingCells attrs) = runQueueT $ case msg of
    _ -> HoldingCells <$> liftRunMessage msg attrs
