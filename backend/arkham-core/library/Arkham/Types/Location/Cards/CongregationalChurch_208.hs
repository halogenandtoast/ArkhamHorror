module Arkham.Types.Location.Cards.CongregationalChurch_208
  ( congregationalChurch_208
  , CongregationalChurch_208(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CongregationalChurch_208 = CongregationalChurch_208 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_208 :: CongregationalChurch_208
congregationalChurch_208 = CongregationalChurch_208 $ baseAttrs
  "02208"
  (Name "Congregational Church" Nothing)
  EncounterSet.BloodOnTheAltar
  1
  (PerPlayer 1)
  Diamond
  [Plus, Triangle, Squiggle]
  [Dunwich]

instance HasModifiersFor env CongregationalChurch_208 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CongregationalChurch_208 where
  getActions iid window (CongregationalChurch_208 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env CongregationalChurch_208 where
  runMessage msg (CongregationalChurch_208 attrs) =
    CongregationalChurch_208 <$> runMessage msg attrs
