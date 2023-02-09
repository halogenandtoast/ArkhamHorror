module Arkham.Event.Cards.TrialByFire
  ( trialByFire
  , TrialByFire(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype TrialByFire = TrialByFire EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trialByFire :: EventCard TrialByFire
trialByFire = event TrialByFire Cards.trialByFire

instance RunMessage TrialByFire where
  runMessage msg e@(TrialByFire attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ chooseOne
          iid
          [ SkillLabel
              skill
              [ CreateWindowModifierEffect
                  EffectTurnWindow
                  (EffectModifiers $ toModifiers attrs [BaseSkillOf skill 5])
                  (toSource attrs)
                  (InvestigatorTarget iid)
              ]
          | skill <- allSkills
          ]
        , discard attrs
        ]
      pure e
    _ -> TrialByFire <$> runMessage msg attrs
