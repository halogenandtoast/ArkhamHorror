module Arkham.Treachery.Cards.EagerForDeathUnionAndDisillusion
  ( EagerForDeathUnionAndDisillusion(..)
  , eagerForDeathUnionAndDisillusion
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message hiding (InvestigatorDamage)
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype EagerForDeathUnionAndDisillusion = EagerForDeathUnionAndDisillusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eagerForDeathUnionAndDisillusion :: TreacheryCard EagerForDeathUnionAndDisillusion
eagerForDeathUnionAndDisillusion = treachery EagerForDeathUnionAndDisillusion Cards.eagerForDeathUnionAndDisillusion

instance RunMessage EagerForDeathUnionAndDisillusion where
  runMessage msg t@(EagerForDeathUnionAndDisillusion attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      difficulty <- fieldMap InvestigatorDamage (+ 2) iid
      t <$ push (RevelationSkillTest iid source SkillWillpower difficulty)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t
      <$ push (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> EagerForDeathUnionAndDisillusion <$> runMessage msg attrs
