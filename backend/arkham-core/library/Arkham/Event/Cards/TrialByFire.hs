module Arkham.Event.Cards.TrialByFire (
  trialByFire,
  TrialByFire (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.SkillType

newtype TrialByFire = TrialByFire EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trialByFire :: EventCard TrialByFire
trialByFire = event TrialByFire Cards.trialByFire

instance RunMessage TrialByFire where
  runMessage msg e@(TrialByFire attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [SkillLabel skill [turnModifier iid attrs iid (BaseSkillOf skill 5)] | skill <- allSkills]
      pure e
    _ -> TrialByFire <$> runMessage msg attrs
