module Arkham.Treachery.Cards.MarkedForDeath (
  markedForDeath,
  MarkedForDeath (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MarkedForDeath = MarkedForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForDeath :: TreacheryCard MarkedForDeath
markedForDeath = treachery MarkedForDeath Cards.markedForDeath

instance RunMessage MarkedForDeath where
  runMessage msg t@(MarkedForDeath attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push
        $ RevelationSkillTest
          iid
          source
          SkillAgility
          (SumDifficulty [Fixed 2, InvestigatorFieldDifficulty iid InvestigatorHorror])
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push (InvestigatorAssignDamage iid source DamageAny 2 0)
          pure t
    _ -> MarkedForDeath <$> runMessage msg attrs
