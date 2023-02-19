module Arkham.Effect.Effects.BlindingLight
  ( blindingLight
  , BlindingLight(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window qualified as Window

newtype BlindingLight = BlindingLight EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EffectArgs -> BlindingLight
blindingLight = BlindingLight . uncurry4 (baseAttrs "01066")

instance RunMessage BlindingLight where
  runMessage msg e@(BlindingLight attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [LoseActions iid (toSource attrs) 1]
          , DisableEffect effectId
          ]
        )
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> e <$ pushAll
        [ EnemyDamage eid $ nonAttack (InvestigatorSource iid) 1
        , DisableEffect effectId
        ]
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> BlindingLight <$> runMessage msg attrs
