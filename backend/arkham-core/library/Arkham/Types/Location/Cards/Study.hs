{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Study where

import Arkham.Import
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Study = Study Attrs
  deriving newtype (Show, ToJSON, FromJSON)

study :: Study
study = Study $ baseAttrs "01111" "Study" 2 (PerPlayer 2) Circle mempty mempty

instance HasModifiersFor env Study where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Study where
  getActions i window (Study attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
