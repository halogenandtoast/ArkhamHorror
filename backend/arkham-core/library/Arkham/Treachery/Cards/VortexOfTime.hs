module Arkham.Treachery.Cards.VortexOfTime (vortexOfTime, VortexOfTime (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
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
      investigatorsAtSentinelHills <- select $ InvestigatorAt $ LocationWithTrait SentinelHill
      sid <- getRandom
      pushAll
        [ revelationSkillTest sid iid source #willpower (Fixed 4)
        | iid <- investigatorsAtSentinelHills
        ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ assignDamage iid attrs 2
      pure t
    _ -> VortexOfTime <$> runMessage msg attrs
