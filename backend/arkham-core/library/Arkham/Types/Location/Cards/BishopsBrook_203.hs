module Arkham.Types.Location.Cards.BishopsBrook_203
  ( bishopsBrook_203
  , BishopsBrook_203(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype BishopsBrook_203 = BishopsBrook_203 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bishopsBrook_203 :: BishopsBrook_203
bishopsBrook_203 = BishopsBrook_203 $ baseAttrs
  "02203"
  (Name "Bishop's Brook" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (Static 2)
  Square
  [Plus, Circle, Triangle]
  [Dunwich]

instance HasModifiersFor env BishopsBrook_203 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BishopsBrook_203 where
  getActions iid window (BishopsBrook_203 attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BishopsBrook_203 where
  runMessage msg (BishopsBrook_203 attrs) =
    BishopsBrook_203 <$> runMessage msg attrs
