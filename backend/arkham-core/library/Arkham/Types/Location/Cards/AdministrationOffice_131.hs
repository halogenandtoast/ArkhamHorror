module Arkham.Types.Location.Cards.AdministrationOffice_131
  ( administrationOffice_131
  , AdministrationOffice_131(..)
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

newtype AdministrationOffice_131 = AdministrationOffice_131 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationOffice_131 :: AdministrationOffice_131
administrationOffice_131 = AdministrationOffice_131 $ baseAttrs
  "02131"
  (Name "Administration Office" Nothing)
  EncounterSet.TheMiskatonicMuseum
  2
  (PerPlayer 2)
  Triangle
  [Square]
  (singleton Miskatonic)

instance HasCount CardCount env InvestigatorId => HasModifiersFor env AdministrationOffice_131 where
  getModifiersFor (InvestigatorSource iid) target (AdministrationOffice_131 attrs)
    | isTarget attrs target
    = do
      cardsInHand <- unCardCount <$> getCount iid
      pure $ toModifiers attrs [ CannotInvestigate | cardsInHand <= 4 ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AdministrationOffice_131 where
  getActions iid window (AdministrationOffice_131 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env AdministrationOffice_131 where
  runMessage msg (AdministrationOffice_131 attrs) =
    AdministrationOffice_131 <$> runMessage msg attrs
