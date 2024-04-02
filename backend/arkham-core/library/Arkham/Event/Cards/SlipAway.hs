module Arkham.Event.Cards.SlipAway (slipAway, slipAwayEffect, SlipAway (..)) where

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude
import Arkham.SkillType

newtype SlipAway = SlipAway EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway :: EventCard SlipAway
slipAway = event SlipAway Cards.slipAway

instance RunMessage SlipAway where
  runMessage msg e@(SlipAway attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      chooseEvade <- toMessage <$> mkChooseEvade iid attrs
      pushAll
        [ skillTestModifier attrs iid (AddSkillValue SkillAgility)
        , chooseEvade
        ]
      pure e
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n | n >= 2 -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just target@(EnemyTarget enemyId) -> do
          nonElite <- enemyId <=~> NonEliteEnemy
          when nonElite $ push $ createCardEffect Cards.slipAway Nothing attrs target
        _ -> error "Invalid call, expected enemy skill test target"
      pure e
    _ -> SlipAway <$> runMessage msg attrs

newtype SlipAwayEffect = SlipAwayEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAwayEffect :: EffectArgs -> SlipAwayEffect
slipAwayEffect = cardEffect SlipAwayEffect Cards.slipAway

instance HasModifiersFor SlipAwayEffect where
  getModifiersFor target (SlipAwayEffect a) | effectTarget a == target = do
    phase <- getPhase
    pure $ toModifiers a [DoesNotReadyDuringUpkeep | phase == UpkeepPhase]
  getModifiersFor _ _ = pure []

instance RunMessage SlipAwayEffect where
  runMessage msg e@(SlipAwayEffect attrs@EffectAttrs {..}) = case msg of
    EndUpkeep -> do
      push (DisableEffect effectId)
      pure e
    _ -> SlipAwayEffect <$> runMessage msg attrs
