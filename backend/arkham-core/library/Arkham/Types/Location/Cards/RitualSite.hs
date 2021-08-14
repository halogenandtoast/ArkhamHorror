module Arkham.Types.Location.Cards.RitualSite where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (ritualSite)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query

newtype RitualSite = RitualSite LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSite :: LocationCard RitualSite
ritualSite =
  location RitualSite Cards.ritualSite 3 (PerPlayer 2) Plus [Squiggle]

instance HasModifiersFor env RitualSite

instance ActionRunner env => HasAbilities env RitualSite where
  getAbilities i window (RitualSite attrs) = getAbilities i window attrs

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
