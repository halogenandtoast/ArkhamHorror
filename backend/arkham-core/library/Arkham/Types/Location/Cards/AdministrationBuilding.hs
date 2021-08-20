module Arkham.Types.Location.Cards.AdministrationBuilding where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (administrationBuilding)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message

newtype AdministrationBuilding = AdministrationBuilding LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationBuilding :: LocationCard AdministrationBuilding
administrationBuilding = location
  AdministrationBuilding
  Cards.administrationBuilding
  4
  (PerPlayer 1)
  Circle
  [Plus, T]

instance HasModifiersFor env AdministrationBuilding

instance HasAbilities env AdministrationBuilding where
  getAbilities i window (AdministrationBuilding attrs) =
    getAbilities i window attrs

instance (LocationRunner env) => RunMessage env AdministrationBuilding where
  runMessage msg l@(AdministrationBuilding attrs) = case msg of
    RevealLocation _ lid | lid == locationId attrs -> do
      push $ PlaceLocationMatching (LocationWithTitle "Faculty Offices")
      AdministrationBuilding <$> runMessage msg attrs
    EndTurn iid | iid `elem` locationInvestigators attrs ->
      l <$ push (DiscardTopOfDeck iid 1 Nothing)
    _ -> AdministrationBuilding <$> runMessage msg attrs
