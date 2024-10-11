module Arkham.Event.Events.TrialByFire3 (trialByFire3, TrialByFire3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier
import Arkham.SkillType

newtype TrialByFire3 = TrialByFire3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trialByFire3 :: EventCard TrialByFire3
trialByFire3 = event TrialByFire3 Cards.trialByFire3

instance RunMessage TrialByFire3 where
  runMessage msg e@(TrialByFire3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      chooseOneM iid do
        for_ (eachWithRest allSkills) \(skill, rest) -> do
          skillLabeled skill $ turnModifiers iid attrs iid $ BaseSkillOf skill 7 : map (`BaseSkillOf` 5) rest
      pure e
    _ -> TrialByFire3 <$> liftRunMessage msg attrs
