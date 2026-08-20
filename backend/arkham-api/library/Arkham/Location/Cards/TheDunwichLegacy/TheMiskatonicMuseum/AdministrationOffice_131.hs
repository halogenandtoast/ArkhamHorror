module Arkham.Location.Cards.TheDunwichLegacy.TheMiskatonicMuseum.AdministrationOffice_131 (administrationOffice_131) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TheDunwichLegacy.TheMiskatonicMuseum qualified as Cards (
  administrationOffice_131,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AdministrationOffice_131 = AdministrationOffice_131 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

administrationOffice_131 :: LocationCard AdministrationOffice_131
administrationOffice_131 = location AdministrationOffice_131 Cards.administrationOffice_131 2 (PerPlayer 2)

instance HasModifiersFor AdministrationOffice_131 where
  getModifiersFor (AdministrationOffice_131 a) =
    whenRevealed a $ modifySelect a (HandWith $ LengthIs $ atMost 4) [CannotInvestigateLocation a.id]

instance RunMessage AdministrationOffice_131 where
  runMessage msg (AdministrationOffice_131 attrs) =
    AdministrationOffice_131 <$> runMessage msg attrs
