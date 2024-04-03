module Arkham.Event.Cards.SlipAway2 (slipAway2, slipAway2Effect, SlipAway2 (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Prelude

newtype SlipAway2 = SlipAway2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway2 :: EventCard SlipAway2
slipAway2 = event SlipAway2 Cards.slipAway2

instance RunMessage SlipAway2 where
  runMessage msg e@(SlipAway2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      chooseEvade <- toMessage <$> mkChooseEvade iid attrs
      pushAll [skillTestModifier attrs iid (AddSkillValue #agility), chooseEvade]
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n >= 1 -> do
      pushWhen (n >= 3)
        $ createCardEffect Cards.slipAway2 (Just $ EffectMetaTarget (toTarget $ toCardId attrs)) attrs iid
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (EnemyTarget enemyId) -> do
          nonElite <- enemyId <=~> NonEliteEnemy
          pushWhen nonElite $ nextPhaseModifier #upkeep attrs enemyId DoesNotReadyDuringUpkeep
        _ -> error "Invalid call, expected enemy skill test target"
      pure e
    _ -> SlipAway2 <$> runMessage msg attrs

newtype SlipAway2Effect = SlipAway2Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slipAway2Effect :: EffectArgs -> SlipAway2Effect
slipAway2Effect = cardEffect SlipAway2Effect Cards.slipAway2

instance RunMessage SlipAway2Effect where
  runMessage msg e@(SlipAway2Effect attrs@EffectAttrs {..}) = case msg of
    EndTurn iid | toTarget iid == attrs.target -> do
      case effectMetadata of
        Just (EffectMetaTarget (CardIdTarget cardId)) -> pushAll [disable attrs, ReturnToHand iid (toTarget cardId)]
        _ -> error "invalid meta target"
      pure e
    _ -> SlipAway2Effect <$> runMessage msg attrs
