module Arkham.Event.Cards.BindMonster2 (bindMonster2, bindMonster2Effect, BindMonster2 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards (bindMonster2)
import Arkham.Event.Runner
import Arkham.Exception
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Elite))

newtype BindMonster2 = BindMonster2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EventCard BindMonster2
bindMonster2 = event BindMonster2 Cards.bindMonster2

instance HasAbilities BindMonster2 where
  getAbilities (BindMonster2 x) = case eventAttachedTarget x of
    Just (EnemyTarget eid) ->
      [ restrictedAbility x 1 ControlsThis
          $ freeReaction (EnemyWouldReady #when $ EnemyWithId eid)
      ]
    _ -> []

instance RunMessage BindMonster2 where
  runMessage msg e@(BindMonster2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      chooseEvade <-
        leftOr <$> aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvade iid attrs)
      pushAll
        $ createCardEffect Cards.bindMonster2 Nothing attrs SkillTestTarget
        : chooseEvade
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 ->
      case eventAttachedTarget attrs of
        Just target -> do
          push $ beginSkillTest iid (attrs.ability 1) target #willpower 3
          pure e
        Nothing -> throwIO $ InvalidState "must be attached"
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      case eventAttachedTarget attrs of
        Just target@(EnemyTarget _) -> withQueue_ (filter (/= Ready target))
        _ -> error "invalid target"
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> BindMonster2 <$> runMessage msg attrs

newtype BindMonster2Effect = BindMonster2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2Effect :: EffectArgs -> BindMonster2Effect
bindMonster2Effect = cardEffect BindMonster2Effect Cards.bindMonster2

instance RunMessage BindMonster2Effect where
  runMessage msg e@(BindMonster2Effect attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget ->
          case effectSource of
            (EventSource evid) -> do
              nonElite <- notMember Elite <$> field EnemyTraits eid
              when
                nonElite
                $ pushAll
                  [PlaceEvent iid evid (AttachedToEnemy eid), DisableEffect effectId]
              pure e
            _ -> pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> BindMonster2Effect <$> runMessage msg attrs
