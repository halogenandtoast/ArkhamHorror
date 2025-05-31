module Arkham.Treachery.Cards.MarkedByTheSign (markedByTheSign) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MarkedByTheSign = MarkedByTheSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedByTheSign :: TreacheryCard MarkedByTheSign
markedByTheSign = treachery MarkedByTheSign Cards.markedByTheSign

instance RunMessage MarkedByTheSign where
  runMessage msg t@(MarkedByTheSign attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      theManInThePallidMaskIsInPlay <- selectAny $ enemyIs Cards.theManInThePallidMask
      let difficulty = if theManInThePallidMaskIsInPlay then 4 else 2
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed difficulty)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      theManInThePallidMaskIsInPlay <- selectAny $ enemyIs Cards.theManInThePallidMask
      if theManInThePallidMaskIsInPlay
        then directHorror iid attrs 2
        else assignHorror iid attrs 2
      pure t
    _ -> MarkedByTheSign <$> liftRunMessage msg attrs
