module Arkham.Event.Events.MysteriesRemain (mysteriesRemain) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues, getJustLocation)
import Arkham.Message qualified as Msg

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
      did <- getRandom

      chooseOrRunOne iid
        $ [ Label "$cards.label.mysteriesRemain.placeClue" [PlaceClues (toSource attrs) (toTarget location) 1]
          ]
        <> [ Label "$label.discoverAtYourLocation count=i:1" [Msg.DiscoverClues iid $ discoverPure did location attrs 1]
           | canDiscover
           ]

      pure e
    _ -> MysteriesRemain <$> liftRunMessage msg attrs
