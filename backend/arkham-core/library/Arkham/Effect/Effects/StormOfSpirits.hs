module Arkham.Effect.Effects.StormOfSpirits (
  StormOfSpirits (..),
  stormOfSpirits,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message
import Arkham.Window qualified as Window

newtype StormOfSpirits = StormOfSpirits EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EffectArgs -> StormOfSpirits
stormOfSpirits = StormOfSpirits . uncurry4 (baseAttrs "03153")

instance RunMessage StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget attrs -> do
      when
        (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        do
          iids <- selectList $ InvestigatorAt $ locationWithInvestigator iid
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token (toId attrs))
                [ InvestigatorAssignDamage iid' (effectSource attrs) DamageAny 1 0
                | iid' <- iids
                ]
            , DisableEffect $ toId attrs
            ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> StormOfSpirits <$> runMessage msg attrs
