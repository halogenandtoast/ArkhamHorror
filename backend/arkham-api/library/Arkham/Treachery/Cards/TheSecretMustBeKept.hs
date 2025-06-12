module Arkham.Treachery.Cards.TheSecretMustBeKept (theSecretMustBeKept) where

import Arkham.Matcher
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheSecretMustBeKept = TheSecretMustBeKept TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretMustBeKept :: TreacheryCard TheSecretMustBeKept
theSecretMustBeKept = treachery TheSecretMustBeKept Cards.theSecretMustBeKept

instance RunMessage TheSecretMustBeKept where
  runMessage msg t@(TheSecretMustBeKept attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- max 3 decks so we subtract the number of decks in play from 3
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 3, SubtractCalculation (Fixed 3) (CountActs AnyAct)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      deckCount <- getActDecksInPlayCount
      let n = 3 - deckCount
      assignDamageAndHorror iid attrs (1 + n) (1 + n)
      pure t
    _ -> TheSecretMustBeKept <$> liftRunMessage msg attrs
