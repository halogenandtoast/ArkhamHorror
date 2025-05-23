module Arkham.Treachery.Cards.UmordhothsHunger (umordhothsHunger) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UmordhothsHunger = UmordhothsHunger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsHunger :: TreacheryCard UmordhothsHunger
umordhothsHunger = treachery UmordhothsHunger Cards.umordhothsHunger

instance RunMessage UmordhothsHunger where
  runMessage msg t@(UmordhothsHunger attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      eachInvestigator \iid -> do
        handCount <- fieldMap InvestigatorHand length iid
        if handCount == 0
          then kill attrs iid
          else randomDiscard iid attrs
      selectEach AnyInPlayEnemy \enemy -> healDamage enemy attrs 1
      pure t
    _ -> UmordhothsHunger <$> liftRunMessage msg attrs
