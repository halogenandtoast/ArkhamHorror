module Arkham.Types.Location.Cards.AdministrationOffice_130
  ( administrationOffice_130
  , AdministrationOffice_130(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (administrationOffice_130)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source

newtype AdministrationOffice_130 = AdministrationOffice_130 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationOffice_130 :: LocationId -> AdministrationOffice_130
administrationOffice_130 = AdministrationOffice_130 . baseAttrs
  Cards.administrationOffice_130
  1
  (PerPlayer 1)
  Triangle
  [Square]

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
