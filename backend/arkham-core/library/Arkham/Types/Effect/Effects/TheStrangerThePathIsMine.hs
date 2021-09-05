module Arkham.Types.Effect.Effects.TheStrangerThePathIsMine
  ( TheStrangerThePathIsMine(..)
  , theStrangerThePathIsMine
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype TheStrangerThePathIsMine = TheStrangerThePathIsMine EffectAttrs
  deriving anyclass (HasModifiersFor env)
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

instance HasQueue env => RunMessage env TheStrangerThePathIsMine where
  runMessage msg e@(TheStrangerThePathIsMine attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _ | isSource attrs source ->
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
