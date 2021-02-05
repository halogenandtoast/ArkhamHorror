module Arkham.Types.Location.Cards.TheHiddenChamber
  ( theHiddenChamber
  , TheHiddenChamber(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TheHiddenChamber = TheHiddenChamber LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHiddenChamber :: TheHiddenChamber
theHiddenChamber = TheHiddenChamber $ base { locationVictory = Just 2 }
 where
  base = baseAttrs
    "02214"
    (Name "The Hidden Chamber" (Just "Prison of the Beast"))
    EncounterSet.BloodOnTheAltar
    3
    (Static 0)
    NoSymbol
    []
    [Dunwich]

instance HasModifiersFor env TheHiddenChamber where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TheHiddenChamber where
  getActions iid window (TheHiddenChamber attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TheHiddenChamber where
  runMessage msg (TheHiddenChamber attrs) =
    TheHiddenChamber <$> runMessage msg attrs
