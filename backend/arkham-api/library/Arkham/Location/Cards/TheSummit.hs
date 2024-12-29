module Arkham.Location.Cards.TheSummit (theSummit) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheSummit = TheSummit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSummit :: LocationCard TheSummit
theSummit = locationWith TheSummit Cards.theSummit 3 (PerPlayer 3) (connectsToL .~ adjacentLocations)

instance HasAbilities TheSummit where
  getAbilities (TheSummit attrs) =
    extendRevealed attrs []

instance HasModifiersFor TheSummit where
  getModifiersFor (TheSummit l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage TheSummit where
  runMessage msg (TheSummit attrs) = runQueueT $ case msg of
    _ -> TheSummit <$> liftRunMessage msg attrs
