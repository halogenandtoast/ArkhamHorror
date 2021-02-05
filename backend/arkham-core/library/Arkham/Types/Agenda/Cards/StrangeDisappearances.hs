module Arkham.Types.Agenda.Cards.StrangeDisappearances
  ( StrangeDisappearances(..)
  , strangeDisappearances
  ) where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype StrangeDisappearances = StrangeDisappearances AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeDisappearances :: StrangeDisappearances
strangeDisappearances = StrangeDisappearances
  $ baseAttrs "02196" "Strange Disappearances" (Agenda 1 A) (Static 6)

instance HasModifiersFor env StrangeDisappearances where
  getModifiersFor = noModifiersFor

instance HasActions env StrangeDisappearances where
  getActions i window (StrangeDisappearances x) = getActions i window x

instance AgendaRunner env => RunMessage env StrangeDisappearances where
  runMessage msg (StrangeDisappearances attrs) =
    StrangeDisappearances <$> runMessage msg attrs
