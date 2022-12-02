module Arkham.Event.Cards.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards (bindMonster2)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Event.Runner
import Arkham.Exception
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype BindMonster2 = BindMonster2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EventCard BindMonster2
bindMonster2 = event BindMonster2 Cards.bindMonster2

instance HasAbilities BindMonster2 where
  getAbilities (BindMonster2 x) = case eventAttachedTarget x of
    Just (EnemyTarget eid) ->
      [ restrictedAbility x 1 ControlsThis
          $ ReactionAbility (EnemyWouldReady Timing.When $ EnemyWithId eid) Free
      ]
    _ -> []

instance RunMessage BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ CreateEffect "02031" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy
        iid
        (EventSource eid)
        Nothing
        SkillWillpower
        AnyEnemy
        False
      ]
    SkillTestEnds _ _ ->
      e <$ when (null eventAttachedTarget) (push (Discard $ toTarget attrs))
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      case eventAttachedTarget of
        Just target ->
          e <$ push (BeginSkillTest iid source target Nothing SkillWillpower 3)
        Nothing -> throwIO $ InvalidState "must be attached"
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> case eventAttachedTarget of
        Just target@(EnemyTarget _) ->
          e <$ withQueue_ (filter (/= Ready target))
        _ -> error "invalid target"
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> e <$ push (Discard $ toTarget attrs)
    _ -> BindMonster2 <$> runMessage msg attrs
