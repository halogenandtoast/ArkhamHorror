module Arkham.Event.Cards.TrialByFire3 (
  trialByFire3,
  TrialByFire3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.SkillType

newtype TrialByFire3 = TrialByFire3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

trialByFire3 :: EventCard TrialByFire3
trialByFire3 = event TrialByFire3 Cards.trialByFire3

instance RunMessage TrialByFire3 where
  runMessage msg e@(TrialByFire3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ SkillLabel skill [turnModifiers attrs iid $ BaseSkillOf skill 7 : map (`BaseSkillOf` 5) rest]
          | (skill, rest) <- eachWithRest allSkills
          ]
      pure e
    _ -> TrialByFire3 <$> runMessage msg attrs
