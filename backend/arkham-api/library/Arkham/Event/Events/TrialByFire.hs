module Arkham.Event.Events.TrialByFire (trialByFire, TrialByFire (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier
import Arkham.SkillType

newtype TrialByFire = TrialByFire EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trialByFire :: EventCard TrialByFire
trialByFire = event TrialByFire Cards.trialByFire

instance RunMessage TrialByFire where
  runMessage msg e@(TrialByFire attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      chooseOneM iid do
        for_ allSkills \skill -> do
          skillLabeled skill $ turnModifier iid attrs iid (BaseSkillOf skill 5)
      pure e
    _ -> TrialByFire <$> liftRunMessage msg attrs
