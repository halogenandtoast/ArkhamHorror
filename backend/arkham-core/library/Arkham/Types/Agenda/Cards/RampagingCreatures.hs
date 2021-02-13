module Arkham.Types.Agenda.Cards.RampagingCreatures
  ( RampagingCreatures(..)
  , rampagingCreatures
  )
where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue

newtype RampagingCreatures = RampagingCreatures AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rampagingCreatures :: RampagingCreatures
rampagingCreatures = RampagingCreatures
  $ baseAttrs "02237" "Rampaging Creatures" (Agenda 1 A) (Static 5)

instance HasModifiersFor env RampagingCreatures where
  getModifiersFor = noModifiersFor

instance HasActions env RampagingCreatures where
  getActions i window (RampagingCreatures x) = getActions i window x

instance AgendaRunner env => RunMessage env RampagingCreatures where
  runMessage msg (RampagingCreatures attrs) =
    RampagingCreatures <$> runMessage msg attrs
