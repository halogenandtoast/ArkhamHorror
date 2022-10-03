module Arkham.Effect.Effects.TheStrangerThePathIsMine
  ( TheStrangerThePathIsMine(..)
  , theStrangerThePathIsMine
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

newtype TheStrangerThePathIsMine = TheStrangerThePathIsMine EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerThePathIsMine :: EffectArgs -> TheStrangerThePathIsMine
theStrangerThePathIsMine =
  TheStrangerThePathIsMine . uncurry4 (baseAttrs "03047b")

instance HasAbilities TheStrangerThePathIsMine where
  getAbilities (TheStrangerThePathIsMine attrs) =
    [ mkAbility
          (ProxySource
            (LocationMatcherSource LocationWithAnyHorror)
            (toSource attrs)
          )
          1
        $ ForcedAbility
        $ Leaves Timing.After You ThisLocation
    ]

instance RunMessage TheStrangerThePathIsMine where
  runMessage msg e@(TheStrangerThePathIsMine attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) 1 _ _ | isSource attrs source ->
      e
        <$ push
             (BeginSkillTest
               iid
               source
               (InvestigatorTarget iid)
               Nothing
               SkillAgility
               4
             )
    FailedSkillTest _ _ source (SkillTestInitiatorTarget (InvestigatorTarget iid)) _ _
      | isSource attrs source
      -> e <$ push (InvestigatorAssignDamage iid source DamageAny 1 1)
    _ -> TheStrangerThePathIsMine <$> runMessage msg attrs
