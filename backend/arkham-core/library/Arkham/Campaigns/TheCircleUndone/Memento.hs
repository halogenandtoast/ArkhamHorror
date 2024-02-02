module Arkham.Campaigns.TheCircleUndone.Memento where

import Arkham.Prelude

data Memento
  = MesmerizingFlute
  | RitualComponents
  | ScrapOfTornShadow
  | Gilman'sJournal
  | Keziah'sFormulae
  | WornCrucifix
  | WispOfSpectralMist
  | CornHuskDoll
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)
