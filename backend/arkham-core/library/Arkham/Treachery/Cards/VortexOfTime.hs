module Arkham.Treachery.Cards.VortexOfTime (
  vortexOfTime,
  VortexOfTime (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VortexOfTime = VortexOfTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vortexOfTime :: TreacheryCard VortexOfTime
vortexOfTime = treachery VortexOfTime Cards.vortexOfTime

instance RunMessage VortexOfTime where
  runMessage msg t@(VortexOfTime attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      investigatorsAtSentinelHills <-
        select $ InvestigatorAt $ LocationWithTrait SentinelHill
      pushAll
        [ RevelationSkillTest iid source SkillWillpower (Fixed 4)
        | iid <- investigatorsAtSentinelHills
        ]
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push $ InvestigatorAssignDamage iid source DamageAny 2 0
          pure t
    _ -> VortexOfTime <$> runMessage msg attrs
