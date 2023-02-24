module Arkham.Effect.Effects.TheStrangerACityAflame
  ( TheStrangerACityAflame(..)
  , theStrangerACityAflame
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype TheStrangerACityAflame = TheStrangerACityAflame EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflame :: EffectArgs -> TheStrangerACityAflame
theStrangerACityAflame = TheStrangerACityAflame . uncurry4 (baseAttrs "03047a")

instance HasAbilities TheStrangerACityAflame where
  getAbilities (TheStrangerACityAflame attrs) =
    [ mkAbility
          (ProxySource
            (LocationMatcherSource LocationWithAnyHorror)
            (toSource attrs)
          )
          1
          (ForcedAbility $ OrWindowMatcher
            [ Enters Timing.After You ThisLocation
            , TurnEnds Timing.When (You <> InvestigatorAt ThisLocation)
            ]
          )
        & abilityLimitL
        .~ PlayerLimit PerRound 1
    ]

instance RunMessage TheStrangerACityAflame where
  runMessage msg e@(TheStrangerACityAflame attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source (InvestigatorTarget iid) SkillAgility 3
      pure e
    FailedSkillTest _ _ source (SkillTestInitiatorTarget (InvestigatorTarget iid)) _ _
      | isSource attrs source
      -> e <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> TheStrangerACityAflame <$> runMessage msg attrs
