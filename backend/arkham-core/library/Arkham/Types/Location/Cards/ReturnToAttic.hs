{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ReturnToAttic where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype ReturnToAttic = ReturnToAttic Attrs
  deriving newtype (Show, ToJSON, FromJSON)

returnToAttic :: ReturnToAttic
returnToAttic = ReturnToAttic
  $ baseAttrs "50018" "Attic" 3 (PerPlayer 1) Triangle [Square, Moon] mempty

instance HasModifiersFor env ReturnToAttic where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ReturnToAttic where
  getActions i window (ReturnToAttic attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ReturnToAttic where
  runMessage msg (ReturnToAttic attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessage (PlaceLocation "50019")
      ReturnToAttic <$> runMessage msg attrs
    _ -> ReturnToAttic <$> runMessage msg attrs
