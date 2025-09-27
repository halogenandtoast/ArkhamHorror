module Arkham.Campaigns.TheCircleUndone.Memento where

import Arkham.Prelude

data Memento
  = MesmerizingFlute
  | StrangeIncantation
  | RitualComponents
  | ScrapOfTornShadow
  | Gilman'sJournal
  | Keziah'sFormulae
  | WornCrucifix
  | WispOfSpectralMist
  | CornHuskDoll
  | BloodyTreeCarvings
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
