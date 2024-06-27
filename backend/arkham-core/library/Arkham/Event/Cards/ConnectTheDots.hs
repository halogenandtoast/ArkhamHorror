module Arkham.Event.Cards.ConnectTheDots (connectTheDots, ConnectTheDots (..)) where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype ConnectTheDots = ConnectTheDots EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

connectTheDots :: EventCard ConnectTheDots
connectTheDots = event ConnectTheDots Cards.connectTheDots

instance RunMessage ConnectTheDots where
  runMessage msg e@(ConnectTheDots attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <-
        select
          $ LocationWithLowerPrintedShroudThan (locationWithInvestigator iid)
          <> locationWithDiscoverableCluesBy iid
      chooseOrRunOne
        iid
        [ targetLabel location [Msg.DiscoverClues iid $ discover location attrs 2]
        | location <- locations
        ]
      pure e
    _ -> ConnectTheDots <$> lift (runMessage msg attrs)
