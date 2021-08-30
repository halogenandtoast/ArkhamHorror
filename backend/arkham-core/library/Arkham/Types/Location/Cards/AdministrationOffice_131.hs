module Arkham.Types.Location.Cards.AdministrationOffice_131
  ( administrationOffice_131
  , AdministrationOffice_131(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (administrationOffice_131)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source

newtype AdministrationOffice_131 = AdministrationOffice_131 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

administrationOffice_131 :: LocationCard AdministrationOffice_131
administrationOffice_131 = location
  AdministrationOffice_131
  Cards.administrationOffice_131
  2
  (PerPlayer 2)
  Triangle
  [Square]

instance HasCount CardCount env InvestigatorId => HasModifiersFor env AdministrationOffice_131 where
  getModifiersFor (InvestigatorSource iid) target (AdministrationOffice_131 attrs)
    | isTarget attrs target
    = do
      cardsInHand <- unCardCount <$> getCount iid
      pure $ toModifiers attrs [ CannotInvestigate | cardsInHand <= 4 ]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage env AdministrationOffice_131 where
  runMessage msg (AdministrationOffice_131 attrs) =
    AdministrationOffice_131 <$> runMessage msg attrs
