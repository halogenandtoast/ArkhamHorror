module Arkham.Treachery.Cards.DescentIntoMadness (descentIntoMadness) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DescentIntoMadness = DescentIntoMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentIntoMadness :: TreacheryCard DescentIntoMadness
descentIntoMadness = treachery DescentIntoMadness Cards.descentIntoMadness

instance RunMessage DescentIntoMadness where
  runMessage msg t@(DescentIntoMadness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      horrorCount <- field InvestigatorHorror iid
      when (horrorCount >= 3) (loseActions iid attrs 1)
      pure t
    _ -> DescentIntoMadness <$> liftRunMessage msg attrs
