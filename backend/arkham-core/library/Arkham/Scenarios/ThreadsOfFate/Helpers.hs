module Arkham.Scenarios.ThreadsOfFate.Helpers where

import Arkham.Prelude

import Arkham.Act.Sequence qualified as Act
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher

getActDecksInPlayCount :: (Monad m, HasGame m) => m Int
getActDecksInPlayCount = do
  hasDeckA <- selectAny $ ActOneOf [ActWithSide Act.A, ActWithSide Act.B]
  hasDeckC <- selectAny $ ActOneOf [ActWithSide Act.C, ActWithSide Act.D]
  hasDeckE <- selectAny $ ActOneOf [ActWithSide Act.E, ActWithSide Act.F]
  pure $ count id [hasDeckA, hasDeckC, hasDeckE]

