module Arkham.Location.Cards.TheGreatWebVastWeb (
  theGreatWebVastWeb,
  TheGreatWebVastWeb (..),
)
where

import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheGreatWebVastWeb = TheGreatWebVastWeb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebVastWeb :: LocationCard TheGreatWebVastWeb
theGreatWebVastWeb =
  locationWith
    TheGreatWebVastWeb
    Cards.theGreatWebVastWeb
    3
    (PerPlayer 2)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebVastWeb where
  getAbilities (TheGreatWebVastWeb attrs) =
    extendRevealed attrs []

instance RunMessage TheGreatWebVastWeb where
  runMessage msg (TheGreatWebVastWeb attrs) = runQueueT $ case msg of
    _ -> TheGreatWebVastWeb <$> lift (runMessage msg attrs)
