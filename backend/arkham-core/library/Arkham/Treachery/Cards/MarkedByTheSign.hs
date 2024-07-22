module Arkham.Treachery.Cards.MarkedByTheSign (markedByTheSign, MarkedByTheSign (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkedByTheSign = MarkedByTheSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedByTheSign :: TreacheryCard MarkedByTheSign
markedByTheSign = treachery MarkedByTheSign Cards.markedByTheSign

instance RunMessage MarkedByTheSign where
  runMessage msg t@(MarkedByTheSign attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      theManInThePallidMaskIsInPlay <- selectAny $ enemyIs Cards.theManInThePallidMask
      let difficulty = if theManInThePallidMaskIsInPlay then 4 else 2
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed difficulty)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      theManInThePallidMaskIsInPlay <- selectAny $ enemyIs Cards.theManInThePallidMask
      push
        $ if theManInThePallidMaskIsInPlay
          then InvestigatorDirectDamage iid (toSource attrs) 0 2
          else InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
      pure t
    _ -> MarkedByTheSign <$> runMessage msg attrs
