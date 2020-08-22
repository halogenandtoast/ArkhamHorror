{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.DisruptingTheRitual where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype DisruptingTheRitual = DisruptingTheRitual Attrs
  deriving newtype (Show, ToJSON, FromJSON)

disruptingTheRitual :: DisruptingTheRitual
disruptingTheRitual =
  DisruptingTheRitual $ baseAttrs "01148" "Disrupting the Ritual" "Act 3a"

instance HasActions env investigator DisruptingTheRitual where
  getActions i window (DisruptingTheRitual x) = getActions i window x

instance (ActRunner env) => RunMessage env DisruptingTheRitual where
  runMessage msg (DisruptingTheRitual attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> error "TODO"
    _ -> DisruptingTheRitual <$> runMessage msg attrs
