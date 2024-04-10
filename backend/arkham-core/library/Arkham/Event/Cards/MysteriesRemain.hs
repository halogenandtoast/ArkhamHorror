module Arkham.Event.Cards.MysteriesRemain (mysteriesRemain, MysteriesRemain (..)) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues, getJustLocation)
import Arkham.Message (IsInvestigate (..))

newtype MysteriesRemain = MysteriesRemain EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriesRemain :: EventCard MysteriesRemain
mysteriesRemain = event MysteriesRemain Cards.mysteriesRemain

instance RunMessage MysteriesRemain where
  runMessage msg e@(MysteriesRemain attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      location <- getJustLocation iid
      canDiscover <- getCanDiscoverClues NotInvestigate iid location

      chooseOrRunOne iid
        $ [ Label
              "Place 1 clue (from the token bank) on your location"
              [PlaceClues (toSource attrs) (toTarget location) 1]
          ]
        <> [Label "Discover 1 clue at your location" [toMessage $ discover iid location attrs 1] | canDiscover]

      pure e
    _ -> MysteriesRemain <$> lift (runMessage msg attrs)
