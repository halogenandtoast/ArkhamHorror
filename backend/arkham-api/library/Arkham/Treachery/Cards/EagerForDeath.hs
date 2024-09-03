module Arkham.Treachery.Cards.EagerForDeath (EagerForDeath (..), eagerForDeath) where

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EagerForDeath = EagerForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eagerForDeath :: TreacheryCard EagerForDeath
eagerForDeath = treachery EagerForDeath Cards.eagerForDeath

instance RunMessage EagerForDeath where
  runMessage msg t@(EagerForDeath attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 2, InvestigatorFieldCalculation iid InvestigatorDamage]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ assignHorror iid attrs 2
      pure t
    _ -> EagerForDeath <$> runMessage msg attrs
