{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.IntoTheDarkness where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype IntoTheDarkness = IntoTheDarkness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

intoTheDarkness :: IntoTheDarkness
intoTheDarkness =
  IntoTheDarkness $ baseAttrs "01147" "Into the Darkness" "Act 2a"

instance HasActions env investigator IntoTheDarkness where
  getActions i window (IntoTheDarkness x) = getActions i window x

instance (ActRunner env) => RunMessage env IntoTheDarkness where
  runMessage msg (IntoTheDarkness attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> error "TODO"
    _ -> IntoTheDarkness <$> runMessage msg attrs
