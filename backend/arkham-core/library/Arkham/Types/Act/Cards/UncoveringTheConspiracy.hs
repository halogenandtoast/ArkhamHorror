{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.UncoveringTheConspiracy where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype UncoveringTheConspiracy = UncoveringTheConspiracy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

uncoveringTheConspiracy :: UncoveringTheConspiracy
uncoveringTheConspiracy = UncoveringTheConspiracy
  $ baseAttrs "01123" "Uncovering the Conspiracy" "Act 1a"

instance (ActRunner env) => RunMessage env UncoveringTheConspiracy where
  runMessage msg (UncoveringTheConspiracy attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      void $ error "Not implemented"
      pure
        $ UncoveringTheConspiracy
        $ attrs
        & (sequence .~ "Act 1b")
        & (flipped .~ True)
    _ -> UncoveringTheConspiracy <$> runMessage msg attrs
