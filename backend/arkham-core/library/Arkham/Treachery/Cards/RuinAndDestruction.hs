module Arkham.Treachery.Cards.RuinAndDestruction (ruinAndDestruction, RuinAndDestruction (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RuinAndDestruction = RuinAndDestruction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinAndDestruction :: TreacheryCard RuinAndDestruction
ruinAndDestruction = treachery RuinAndDestruction Cards.ruinAndDestruction

instance RunMessage RuinAndDestruction where
  runMessage msg t@(RuinAndDestruction attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      targetInvestigators <- select $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTitle broodTitle
      pushAll
        $ [revelationSkillTest iid' source #agility (Fixed 3) | iid' <- targetInvestigators]
        <> [gainSurge attrs | null targetInvestigators]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ assignDamage iid attrs n
      pure t
    _ -> RuinAndDestruction <$> runMessage msg attrs
