module Arkham.Event.Events.TrialByFire3 (trialByFire3, TrialByFire3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
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
      chooseOneM iid $ cardI18n $ scope "trialByFire" do
        labeled' "all" do
          turnModifiers iid attrs iid $ map (`BaseSkillOf` 5) allSkills
        labeled' "one" do
          chooseOneM iid do
            for_ allSkills \skill -> do
              skillLabeled skill $ turnModifier iid attrs iid $ BaseSkillOf skill 7
      pure e
    _ -> TrialByFire3 <$> liftRunMessage msg attrs
