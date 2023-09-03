module Arkham.Scenarios.UnionAndDisillusion.Helpers where

import Arkham.Prelude

import Arkham.Field
import Arkham.Id
import Arkham.Location.Brazier
import Arkham.Location.Types
import Arkham.Message

lightBrazier :: LocationId -> Message
lightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Lit)
