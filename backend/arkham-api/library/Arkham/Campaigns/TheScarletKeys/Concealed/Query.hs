module Arkham.Campaigns.TheScarletKeys.Concealed.Query where

import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Campaigns.TheScarletKeys.Modifiers
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Source
import Arkham.Id
import Arkham.Matcher.Location
import Arkham.Matcher.Source
import Arkham.Prelude
import Arkham.Source
import Arkham.Tracing

data ForExpose = ForExpose Source | NotForExpose

getConcealedChoicesAt :: (HasGame m, Tracing m) => ForExpose -> LocationMatcher -> m [ConcealedCard]
getConcealedChoicesAt fe lmatcher = do
  locations <- select lmatcher
  concatForM locations (getConcealedAt fe)

getConcealedAt
  :: (HasGame m, Tracing m, ToId location LocationId) => ForExpose -> location -> m [ConcealedCard]
getConcealedAt fe location = do
  concealed <- getConcealedAtAll fe location
  let (known, unknown) = partition (attr concealedCardKnown) concealed
  pure $ known <> take 1 unknown

getConcealedAtAll
  :: (HasGame m, Tracing m, ToId location LocationId) => ForExpose -> location -> m [ConcealedCard]
getConcealedAtAll fe location = do
  wrap <- case fe of
    ForExpose source -> do
      isPlayerSource <- sourceMatches source SourceIsPlayerCard
      pure $ if isPlayerSource then (<> LocationWithoutModifier NoExposeAt) else id
    NotForExpose -> pure id
  select $ ConcealedCardAt (wrap $ LocationWithId $ asId location)
