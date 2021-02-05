module Arkham.Types.Location.Cards.CongregationalChurch_209
  ( congregationalChurch_209
  , CongregationalChurch_209(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CongregationalChurch_209 = CongregationalChurch_209 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_209 :: CongregationalChurch_209
congregationalChurch_209 = CongregationalChurch_209 $ baseAttrs
  "02209"
  (Name "Congregational Church" Nothing)
  EncounterSet.BloodOnTheAltar
  2
  (PerPlayer 1)
  Diamond
  [Plus, Triangle, Squiggle]
  [Dunwich]

instance HasModifiersFor env CongregationalChurch_209 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CongregationalChurch_209 where
  getActions iid window (CongregationalChurch_209 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env CongregationalChurch_209 where
  runMessage msg (CongregationalChurch_209 attrs) =
    CongregationalChurch_209 <$> runMessage msg attrs
