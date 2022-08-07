module Arkham.Location.Cards.AdministrationOffice_131
  ( administrationOffice_131
  , AdministrationOffice_131(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards ( administrationOffice_131 )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.Target

newtype AdministrationOffice_131 = AdministrationOffice_131 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

administrationOffice_131 :: LocationCard AdministrationOffice_131
administrationOffice_131 = location
  AdministrationOffice_131
  Cards.administrationOffice_131
  2
  (PerPlayer 2)

instance HasModifiersFor AdministrationOffice_131 where
  getModifiersFor (InvestigatorTarget iid) (AdministrationOffice_131 attrs) =
    do
      cardsInHand <- fieldMap InvestigatorHand length iid
      pure $ toModifiers
        attrs
        [ CannotInvestigateLocation (toId attrs) | cardsInHand <= 4 ]
  getModifiersFor _ _ = pure []

instance RunMessage AdministrationOffice_131 where
  runMessage msg (AdministrationOffice_131 attrs) =
    AdministrationOffice_131 <$> runMessage msg attrs
