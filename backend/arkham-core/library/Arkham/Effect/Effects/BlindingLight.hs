module Arkham.Effect.Effects.BlindingLight (
  blindingLight,
  BlindingLight (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.Window qualified as Window

newtype BlindingLight = BlindingLight EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EffectArgs -> BlindingLight
blindingLight = BlindingLight . uncurry4 (baseAttrs "01066")

instance RunMessage BlindingLight where
  runMessage msg e@(BlindingLight attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [LoseActions iid (toSource attrs) 1]
          , DisableEffect effectId
          ]
      pure e
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget -> do
          pushAll
            [ EnemyDamage eid $ nonAttack (InvestigatorSource iid) 1
            , DisableEffect effectId
            ]
          pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> BlindingLight <$> runMessage msg attrs
