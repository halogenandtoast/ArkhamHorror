module Arkham.Location.Cards.ForkedPath (forkedPath) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ForkedPath = ForkedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkedPath :: LocationCard ForkedPath
forkedPath = symbolLabel $ location ForkedPath Cards.forkedPath 2 (PerPlayer 2)

-- Forced - When you explore while at Forked Path, check your supplies. If you
-- have a map: Continue drawing cards from the exploration deck until you have
-- drawn 2 connecting locations, if able. Put both of those locations into play
-- and choose 1 to move to

instance HasAbilities ForkedPath where
  getAbilities (ForkedPath a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> InvestigatorExists (You <> InvestigatorWithSupply Map))
      $ forced
      $ AttemptExplore #when You

instance RunMessage ForkedPath where
  runMessage msg l@(ForkedPath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      matchingDon't \case
        Do (Explore {}) -> True
        _ -> False
      explore
        iid
        (toSource attrs)
        (CardWithPrintedLocationConnection $ locationSymbol attrs)
        PlaceExplored
        2
      pure l
    _ -> ForkedPath <$> liftRunMessage msg attrs
