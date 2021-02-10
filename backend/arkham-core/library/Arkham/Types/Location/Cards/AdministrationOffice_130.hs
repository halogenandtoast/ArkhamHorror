module Arkham.Types.Location.Cards.AdministrationOffice_130
  ( administrationOffice_130
  , AdministrationOffice_130(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AdministrationOffice_130 = AdministrationOffice_130 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationOffice_130 :: AdministrationOffice_130
administrationOffice_130 = AdministrationOffice_130 $ baseAttrs
  "02130"
  (Name "Administration Office" Nothing)
  EncounterSet.TheMiskatonicMuseum
  1
  (PerPlayer 1)
  Triangle
  [Square]
  (singleton Miskatonic)

instance HasCount ResourceCount env InvestigatorId => HasModifiersFor env AdministrationOffice_130 where
  getModifiersFor (InvestigatorSource iid) target (AdministrationOffice_130 attrs)
    | isTarget attrs target
    = do
      resources <- unResourceCount <$> getCount iid
      pure $ toModifiers attrs [ CannotInvestigate | resources <= 4 ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AdministrationOffice_130 where
  getActions iid window (AdministrationOffice_130 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env AdministrationOffice_130 where
  runMessage msg (AdministrationOffice_130 attrs) =
    AdministrationOffice_130 <$> runMessage msg attrs
