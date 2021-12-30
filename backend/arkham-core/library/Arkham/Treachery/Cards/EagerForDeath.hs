module Arkham.Treachery.Cards.EagerForDeath
  ( EagerForDeath(..)
  , eagerForDeath
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Query
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype EagerForDeath = EagerForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eagerForDeath :: TreacheryCard EagerForDeath
eagerForDeath = treachery EagerForDeath Cards.eagerForDeath

instance TreacheryRunner env => RunMessage env EagerForDeath where
  runMessage msg t@(EagerForDeath attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      difficulty <- (+ 2) . unDamageCount <$> getCount iid
      t <$ push (RevelationSkillTest iid source SkillWillpower difficulty)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t
      <$ push (InvestigatorAssignDamage iid source DamageAny 0 2)
    _ -> EagerForDeath <$> runMessage msg attrs
