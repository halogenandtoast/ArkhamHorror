module Arkham.Event.Cards.SlipAway2 (
  slipAway2,
  slipAway2Effect,
  SlipAway2 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Phase
import Arkham.SkillType

newtype SlipAway2 = SlipAway2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

slipAway2 :: EventCard SlipAway2
slipAway2 = event SlipAway2 Cards.slipAway2

instance RunMessage SlipAway2 where
  runMessage msg e@(SlipAway2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (AddSkillValue SkillAgility)
        , ChooseEvadeEnemy iid (toSource attrs) Nothing SkillAgility AnyEnemy False
        ]
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n | n >= 1 -> do
      when (n >= 3)
        $ push
        $ createCardEffect Cards.slipAway2 (Just $ EffectMetaTarget (toTarget $ toCardId attrs)) attrs iid
      mTarget <- getSkillTestTarget
      case mTarget of
        Just target@(EnemyTarget enemyId) -> do
          nonElite <- enemyId <=~> NonEliteEnemy
          when nonElite $ push $ createCardEffect Cards.slipAway2 Nothing attrs target
        _ -> error "Invalid call, expected enemy skill test target"
      pure e
    _ -> SlipAway2 <$> runMessage msg attrs

newtype SlipAway2Effect = SlipAway2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

slipAway2Effect :: EffectArgs -> SlipAway2Effect
slipAway2Effect = cardEffect SlipAway2Effect Cards.slipAway2

instance HasModifiersFor SlipAway2Effect where
  getModifiersFor target (SlipAway2Effect a) | effectTarget a == target = do
    phase <- getPhase
    pure $ toModifiers a [DoesNotReadyDuringUpkeep | phase == UpkeepPhase]
  getModifiersFor _ _ = pure []

instance RunMessage SlipAway2Effect where
  runMessage msg e@(SlipAway2Effect attrs@EffectAttrs {..}) = case msg of
    EndUpkeep | isNothing effectMetadata -> do
      push (DisableEffect effectId)
      pure e
    EndTurn iid | toTarget iid == effectTarget -> do
      case effectMetadata of
        Just (EffectMetaTarget (CardIdTarget cardId)) -> pushAll [DisableEffect effectId, ReturnToHand iid (toTarget cardId)]
        _ -> error "invalid meta target"
      pure e
    _ -> SlipAway2Effect <$> runMessage msg attrs
