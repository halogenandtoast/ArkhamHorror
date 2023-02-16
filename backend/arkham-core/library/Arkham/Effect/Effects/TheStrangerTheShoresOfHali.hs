module Arkham.Effect.Effects.TheStrangerTheShoresOfHali
  ( TheStrangerTheShoresOfHali(..)
  , theStrangerTheShoresOfHali
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype TheStrangerTheShoresOfHali = TheStrangerTheShoresOfHali EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHali :: EffectArgs -> TheStrangerTheShoresOfHali
theStrangerTheShoresOfHali =
  TheStrangerTheShoresOfHali . uncurry4 (baseAttrs "03047a")

instance HasAbilities TheStrangerTheShoresOfHali where
  getAbilities (TheStrangerTheShoresOfHali attrs) =
    [ limitedAbility (PlayerLimit PerRound 1)
      $ mkAbility
        (ProxySource
          (LocationMatcherSource LocationWithAnyHorror)
          (toSource attrs)
        )
        1
        (ForcedAbility $ Leaves Timing.When You ThisLocation)
    ]

instance RunMessage TheStrangerTheShoresOfHali where
  runMessage msg e@(TheStrangerTheShoresOfHali attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source (InvestigatorTarget iid) SkillAgility 2
      pure e
    FailedSkillTest _ _ source (SkillTestInitiatorTarget (InvestigatorTarget iid)) _ _
      | isSource attrs source
      -> do
        popMessageMatching_ \case
          MoveFrom _ iid' _ -> iid' == iid
          _ -> False
        popMessageMatching_ \case
          MoveTo _ iid' _ -> iid == iid'
          _ -> False
        e <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> TheStrangerTheShoresOfHali <$> runMessage msg attrs
