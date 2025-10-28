module Arkham.Treachery.Cards.TerrorUnleashed (terrorUnleashed) where

import Arkham.Helpers.Location
import Arkham.I18n
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorUnleashed = TerrorUnleashed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorUnleashed :: TreacheryCard TerrorUnleashed
terrorUnleashed = treachery TerrorUnleashed Cards.terrorUnleashed

instance RunMessage TerrorUnleashed where
  runMessage msg t@(TerrorUnleashed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \location -> do
        breaches <- fieldMap LocationBreaches (maybe 0 countBreaches) location
        doom <- field LocationDoom location
        let x = doom + max 1 breaches
        when (breaches == 0) $ placeBreaches location 1
        repeated x $ chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
          countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
      pure t
    _ -> TerrorUnleashed <$> liftRunMessage msg attrs
