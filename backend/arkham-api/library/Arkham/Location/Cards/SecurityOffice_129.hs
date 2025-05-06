module Arkham.Location.Cards.SecurityOffice_129 (securityOffice_129) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (securityOffice_129)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers

newtype SecurityOffice_129 = SecurityOffice_129 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_129 :: LocationCard SecurityOffice_129
securityOffice_129 = location SecurityOffice_129 Cards.securityOffice_129 3 (PerPlayer 2)

instance HasAbilities SecurityOffice_129 where
  getAbilities (SecurityOffice_129 x) =
    extendRevealed1 x $ playerLimit PerTurn $ restricted x 1 Here doubleActionAbility

instance RunMessage SecurityOffice_129 where
  runMessage msg l@(SecurityOffice_129 attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      unrevealedExhibitHalls <- select $ UnrevealedLocation <> "ExhibitHall"
      chooseOrRunOneM iid do
        labeled' "securityOffice.topOfDeck" $ push $ LookAtTopOfDeck iid ScenarioDeckTarget 1
        targets unrevealedExhibitHalls $ push . LookAtRevealed iid (toSource attrs) . toTarget
      pure l
    _ -> SecurityOffice_129 <$> liftRunMessage msg attrs
