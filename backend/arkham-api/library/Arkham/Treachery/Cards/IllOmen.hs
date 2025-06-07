module Arkham.Treachery.Cards.IllOmen (illOmen) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IllOmen = IllOmen TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illOmen :: TreacheryCard IllOmen
illOmen = treachery IllOmen Cards.illOmen

instance RunMessage IllOmen where
  runMessage msg t@(IllOmen attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ LocationWithInvestigator UneliminatedInvestigator
      chooseTargetM iid locations \lid -> do
        placeDoom attrs lid 1
        selectEach (investigatorAt lid) \i -> assignHorror i attrs 1
      pure t
    _ -> IllOmen <$> liftRunMessage msg attrs
