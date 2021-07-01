module Arkham.Types.Location.Cards.RitualSite where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (ritualSite)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query

newtype RitualSite = RitualSite LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSite :: LocationId -> RitualSite
ritualSite = RitualSite . baseAttrs
  Cards.ritualSite
  3
  (PerPlayer 2)
  Plus
  [Squiggle]

instance HasModifiersFor env RitualSite where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RitualSite where
  getActions i window (RitualSite attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env RitualSite where
  runMessage msg (RitualSite attrs) = case msg of
    EndRound -> do
      playerCount <- getCount ()
      RitualSite <$> runMessage
        msg
        (attrs & cluesL .~ fromGameValue
          (PerPlayer 2)
          (unPlayerCount playerCount)
        )
    _ -> RitualSite <$> runMessage msg attrs
