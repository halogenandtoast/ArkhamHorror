module Arkham.Location.Cards.AdministrationOffice_130 (
  administrationOffice_130,
  AdministrationOffice_130 (..),
) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (administrationOffice_130)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AdministrationOffice_130 = AdministrationOffice_130 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

administrationOffice_130 :: LocationCard AdministrationOffice_130
administrationOffice_130 = location AdministrationOffice_130 Cards.administrationOffice_130 1 (PerPlayer 1)

instance HasModifiersFor AdministrationOffice_130 where
  getModifiersFor (AdministrationOffice_130 attrs) =
    whenRevealed attrs
      $ modifySelect attrs (InvestigatorWithResources $ atMost 4) [CannotInvestigateLocation attrs.id]

instance RunMessage AdministrationOffice_130 where
  runMessage msg (AdministrationOffice_130 attrs) =
    AdministrationOffice_130 <$> runMessage msg attrs
