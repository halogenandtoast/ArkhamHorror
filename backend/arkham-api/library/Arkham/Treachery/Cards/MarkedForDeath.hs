module Arkham.Treachery.Cards.MarkedForDeath (markedForDeath, MarkedForDeath (..)) where

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkedForDeath = MarkedForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForDeath :: TreacheryCard MarkedForDeath
markedForDeath = treachery MarkedForDeath Cards.markedForDeath

instance RunMessage MarkedForDeath where
  runMessage msg t@(MarkedForDeath attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #agility
        $ SumCalculation [Fixed 2, InvestigatorFieldCalculation iid InvestigatorHorror]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ assignDamage iid attrs 2
      pure t
    _ -> MarkedForDeath <$> runMessage msg attrs
