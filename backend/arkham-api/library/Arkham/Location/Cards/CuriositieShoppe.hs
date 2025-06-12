module Arkham.Location.Cards.CuriositieShoppe (curiositieShoppe) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMap)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype CuriositieShoppe = CuriositieShoppe LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

curiositieShoppe :: LocationCard CuriositieShoppe
curiositieShoppe = location CuriositieShoppe Cards.curiositieShoppe 2 (PerPlayer 2)

instance HasModifiersFor CuriositieShoppe where
  getModifiersFor (CuriositieShoppe a) = do
    modifySelectMap a (locationIs Cards.northside) \lid -> [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)]
    modifySelect a (investigatorAt a) [ReduceCostOf (#asset <> CardWithTrait Relic) 2]

instance RunMessage CuriositieShoppe where
  runMessage msg (CuriositieShoppe attrs) = CuriositieShoppe <$> runMessage msg attrs
