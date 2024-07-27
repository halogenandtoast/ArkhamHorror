module Arkham.Event.Cards.Snitch2 (snitch2, Snitch2 (..)) where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (DiscoverClues)

newtype Snitch2 = Snitch2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snitch2 :: EventCard Snitch2
snitch2 = event Snitch2 Cards.snitch2

instance RunMessage Snitch2 where
  runMessage msg e@(Snitch2 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 2 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      locations <-
        select
          $ locationWithDiscoverableCluesBy iid
          <> oneOf [locationWithInvestigator iid, ConnectedFrom (locationWithInvestigator iid)]
      when (notNull locations) do
        chooseOrRunOne
          iid
          [targetLabel location [DiscoverClues iid $ discover location attrs 1] | location <- locations]
      doStep (n - 1) msg'
      pure e
    _ -> Snitch2 <$> liftRunMessage msg attrs
