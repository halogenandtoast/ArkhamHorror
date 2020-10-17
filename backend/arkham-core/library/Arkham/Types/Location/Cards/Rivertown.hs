{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Rivertown where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Rivertown = Rivertown Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rivertown :: Rivertown
rivertown = Rivertown $ baseAttrs
  "01125"
  "Rivertown"
  1
  (PerPlayer 1)
  Circle
  [Moon, Diamond, Square, Squiggle, Hourglass]
  [Arkham, Central]

instance HasModifiersFor env Rivertown where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Rivertown where
  getActions i window (Rivertown attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Rivertown where
  runMessage msg (Rivertown attrs) = Rivertown <$> runMessage msg attrs
