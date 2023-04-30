module Arkham.Campaigns.TheCircleUndone.Memento where

import Arkham.Prelude

data Memento
  = MesmerizingFlute
  | RitualComponents
  | ScrapOfTownShadow
  | Gilman'sJournal
  | Keziah'sFormulae
  | WornCrucifix
  | WispOfSpectralMist
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
