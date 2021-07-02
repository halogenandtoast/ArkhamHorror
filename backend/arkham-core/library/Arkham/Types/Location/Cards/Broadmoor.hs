module Arkham.Types.Location.Cards.Broadmoor
  ( Broadmoor(..)
  , broadmoor
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (broadmoor)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol

newtype Broadmoor = Broadmoor LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broadmoor :: LocationId -> Broadmoor
broadmoor = Broadmoor . baseAttrs
  Cards.broadmoor
  3
  (PerPlayer 1)
  Plus
  [Square, Plus]

instance HasModifiersFor env Broadmoor where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Broadmoor where
  getActions = withResignAction

instance LocationRunner env => RunMessage env Broadmoor where
  runMessage msg (Broadmoor attrs) = Broadmoor <$> runMessage msg attrs
