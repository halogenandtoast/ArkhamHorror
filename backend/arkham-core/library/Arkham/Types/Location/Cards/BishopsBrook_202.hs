module Arkham.Types.Location.Cards.BishopsBrook_202
  ( bishopsBrook_202
  , BishopsBrook_202(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BishopsBrook_202 = BishopsBrook_202 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_202 :: BishopsBrook_202
bishopsBrook_202 = BishopsBrook_202 $ baseAttrs
  "02202"
  (Name "Bishop's Brook" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 2)
  Square
  [Plus, Circle, Triangle]
  [Dunwich]

instance HasModifiersFor env BishopsBrook_202 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BishopsBrook_202 where
  getActions iid window (BishopsBrook_202 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BishopsBrook_202 where
  runMessage msg (BishopsBrook_202 attrs) =
    BishopsBrook_202 <$> runMessage msg attrs
