module Arkham.Types.Location.Cards.DevilsHopYard_252
  ( devilsHopYard_252
  , DevilsHopYard_252(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.Trait

newtype DevilsHopYard_252 = DevilsHopYard_252 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_252 :: DevilsHopYard_252
devilsHopYard_252 = DevilsHopYard_252 $ baseAttrs
  "02252"
  (Name "Devil's Hop Yard" Nothing)
  EncounterSet.UndimensionedAndUnseen
  1
  (Static 2)
  Hourglass
  [Square, Plus]
  [Dunwich]

instance HasModifiersFor env DevilsHopYard_252 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DevilsHopYard_252 where
  getActions iid window (DevilsHopYard_252 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DevilsHopYard_252 where
  runMessage msg (DevilsHopYard_252 attrs) =
    DevilsHopYard_252 <$> runMessage msg attrs
