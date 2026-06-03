module Arkham.Location.Cards.FetidPool (fetidPool) where

import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Token (Token (..), countTokens)

newtype FetidPool = FetidPool LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fetidPool :: LocationCard FetidPool
fetidPool = locationWith FetidPool Cards.fetidPool 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor FetidPool where
  getModifiersFor (FetidPool a) = do
    let sinkholes = countTokens Damage a.tokens
    modifySelfWhen a (sinkholes > 0) [ShroudModifier sinkholes]

instance HasAbilities FetidPool where
  getAbilities (FetidPool a) = extendRevealed a []

instance RunMessage FetidPool where
  runMessage msg l@(FetidPool attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10595b
      pure l
    _ -> FetidPool <$> liftRunMessage msg attrs
