module Arkham.Types.Act.Cards.Row
  ( Row(..)
  , row
  ) where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype Row = Row ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

row :: Row
row = Row $ baseAttrs "82006" "Get to the Boats!" (Act 3 A) Nothing

instance ActionRunner env => HasActions env Row where
  getActions iid window (Row x) = getActions iid window x

instance ActRunner env => RunMessage env Row where
  runMessage msg a@(Row attrs) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> Row <$> runMessage msg attrs
