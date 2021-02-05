module Arkham.Types.Location.Cards.BurnedRuins_204
  ( burnedRuins_204
  , BurnedRuins_204(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BurnedRuins_204 = BurnedRuins_204 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnedRuins_204 :: BurnedRuins_204
burnedRuins_204 = BurnedRuins_204 $ baseAttrs
  "02204"
  (Name "Burned Ruins" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 3)
  Triangle
  [Square, Diamond]
  [Dunwich]

instance HasModifiersFor env BurnedRuins_204 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BurnedRuins_204 where
  getActions iid window (BurnedRuins_204 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BurnedRuins_204 where
  runMessage msg (BurnedRuins_204 attrs) =
    BurnedRuins_204 <$> runMessage msg attrs
