module Arkham.Location.Cards.AdministrationOffice_130
  ( administrationOffice_130
  , AdministrationOffice_130(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards ( administrationOffice_130 )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.Target

newtype AdministrationOffice_130 = AdministrationOffice_130 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

administrationOffice_130 :: LocationCard AdministrationOffice_130
administrationOffice_130 = location
  AdministrationOffice_130
  Cards.administrationOffice_130
  1
  (PerPlayer 1)

instance HasModifiersFor AdministrationOffice_130 where
  getModifiersFor (InvestigatorTarget iid) (AdministrationOffice_130 attrs) =
    do
      resources <- field InvestigatorResources iid
      pure $ toModifiers
        attrs
        [ CannotInvestigateLocation (toId attrs) | resources <= 4 ]
  getModifiersFor _ _ = pure []

instance RunMessage AdministrationOffice_130 where
  runMessage msg (AdministrationOffice_130 attrs) =
    AdministrationOffice_130 <$> runMessage msg attrs
