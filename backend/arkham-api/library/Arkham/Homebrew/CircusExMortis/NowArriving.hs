{- | The trigger side of All Points West's "Now Arriving" interludes. Each
"Arriving at <City>" act back reads one of two interludes depending on how much
doom is in play at that moment; the act calls 'nowArriving' between placing doom
for each enemy and removing all doom, and the scenario resolves the rest.
-}
module Arkham.Homebrew.CircusExMortis.NowArriving (Arrival (..), nowArriving, nowArrivingKey) where

import Arkham.Classes.HasQueue (push)
import Arkham.Message (Message (ScenarioSpecific))
import Arkham.Message.Lifted (ReverseQueue)
import Arkham.Prelude

data Arrival = ArrivingAtChicago | ArrivingAtMemphis | ArrivingAtStLouis | ArrivingAtDenver
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

nowArrivingKey :: Text
nowArrivingKey = "allPointsWest.nowArriving"

nowArriving :: ReverseQueue m => Arrival -> m ()
nowArriving = push . ScenarioSpecific nowArrivingKey . toJSON
