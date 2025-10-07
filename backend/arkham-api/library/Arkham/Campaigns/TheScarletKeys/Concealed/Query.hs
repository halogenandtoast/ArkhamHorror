module Arkham.Campaigns.TheScarletKeys.Concealed.Query where

import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher.Location
import Arkham.Prelude

getConcealedChoicesAt :: HasGame m => LocationMatcher -> m [ConcealedCard]
getConcealedChoicesAt lmatcher = do
  locations <- select lmatcher
  concatForM locations getConcealedAt

getConcealedAt :: (HasGame m, ToId location LocationId) => location -> m [ConcealedCard]
getConcealedAt location = do
  concealed <- getConcealedAtAll location
  let (known, unknown) = partition (attr concealedCardKnown) concealed
  pure $ known <> take 1 unknown

getConcealedAtAll :: (HasGame m, ToId location LocationId) => location -> m [ConcealedCard]
getConcealedAtAll location = select $ ConcealedCardAt (LocationWithId $ asId location)
