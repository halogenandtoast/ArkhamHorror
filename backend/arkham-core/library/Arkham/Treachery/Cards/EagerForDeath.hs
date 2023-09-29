module Arkham.Treachery.Cards.EagerForDeath (
  EagerForDeath (..),
  eagerForDeath,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EagerForDeath = EagerForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eagerForDeath :: TreacheryCard EagerForDeath
eagerForDeath = treachery EagerForDeath Cards.eagerForDeath

instance RunMessage EagerForDeath where
  runMessage msg t@(EagerForDeath attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      difficulty <- fieldMap InvestigatorDamage (+ 2) iid
      push $ RevelationSkillTest iid source SkillWillpower difficulty
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push (InvestigatorAssignDamage iid source DamageAny 0 2)

          pure t
    _ -> EagerForDeath <$> runMessage msg attrs
