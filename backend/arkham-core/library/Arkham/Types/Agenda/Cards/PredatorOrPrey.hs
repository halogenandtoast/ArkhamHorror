{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.PredatorOrPrey where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype PredatorOrPrey = PredatorOrPrey Attrs
  deriving newtype (Show, ToJSON, FromJSON)

predatorOrPrey :: PredatorOrPrey
predatorOrPrey =
  PredatorOrPrey $ baseAttrs "01121" "Predator or Prey?" "Agenda 1a" (Static 6)

instance (AgendaRunner env) => RunMessage env PredatorOrPrey where
  runMessage msg a@(PredatorOrPrey attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      void $ error "Need to spawn the masked hunter"
      pure a
    _ -> PredatorOrPrey <$> runMessage msg attrs
