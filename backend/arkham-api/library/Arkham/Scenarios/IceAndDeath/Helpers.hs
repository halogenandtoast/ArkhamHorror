module Arkham.Scenarios.IceAndDeath.Helpers where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "iceAndDeath" a

placeSetAsideConnectedAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Ability
placeSetAsideConnectedAbility a n = mkAbility a n $ forced $ Enters #after You $ ConnectedToSetAsideLocation

placeSetAsideConnectedLocations :: ReverseQueue m => LocationId -> m ()
placeSetAsideConnectedLocations lid = do
  symbol <- field LocationPrintedSymbol lid
  let isConnected = elem symbol . cdLocationConnections . toCardDef
  setAsideLocations <- filter isConnected <$> getSetAsideCardsMatching #location
  traverse_ placeLocation_ setAsideLocations
