{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.RitualSite where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Trait
import ClassyPrelude
import Lens.Micro

newtype RitualSite = RitualSite Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ritualSite :: RitualSite
ritualSite = RitualSite
  $ baseAttrs "01156" "Ritual Site" 3 (PerPlayer 2) Plus [Squiggle] [Cave]

instance (IsInvestigator investigator) => HasActions env investigator RitualSite where
  getActions i window (RitualSite attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env RitualSite where
  runMessage msg (RitualSite attrs) = case msg of
    EndRound -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      RitualSite <$> runMessage
        msg
        (attrs & clues .~ fromGameValue (PerPlayer 2) playerCount)
    _ -> RitualSite <$> runMessage msg attrs
