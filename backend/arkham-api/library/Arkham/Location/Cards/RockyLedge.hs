module Arkham.Location.Cards.RockyLedge (rockyLedge) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.Shuffle (whenCanShuffleIn)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype RockyLedge = RockyLedge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rockyLedge :: LocationCard RockyLedge
rockyLedge = locationWith RockyLedge Cards.rockyLedge 4 (PerPlayer 1) (connectsToL .~ adjacentLocations)

instance HasAbilities RockyLedge where
  getAbilities (RockyLedge a) =
    extendRevealed1 a
      $ restricted a 1 (ScenarioDeckWithCard TekeliliDeck <> youExist CanShuffleIn)
      $ forced
      $ Moves #after You AnySource (below a) (be a)

instance HasModifiersFor RockyLedge where
  getModifiersFor (RockyLedge l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage RockyLedge where
  runMessage msg l@(RockyLedge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tekelili <- getTekelili 1
      whenCanShuffleIn iid tekelili $ addTekelili iid tekelili
      pure l
    _ -> RockyLedge <$> liftRunMessage msg attrs
