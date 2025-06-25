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
        labeled "Set the base value of each of your skills to 5" do
          turnModifiers iid attrs iid $ map (`BaseSkillOf` 5) allSkills
        labeled "Set the base value of one of your skills to 7" do
          chooseOneM iid do
            for_ allSkills \skill -> do
              skillLabeled skill $ turnModifier iid attrs iid $ BaseSkillOf skill 7
      pure e
    _ -> TrialByFire3 <$> liftRunMessage msg attrs
