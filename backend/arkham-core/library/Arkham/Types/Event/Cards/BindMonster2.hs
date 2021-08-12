module Arkham.Types.Event.Cards.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards (bindMonster2)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Event.Attrs
import Arkham.Types.Exception
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype BindMonster2 = BindMonster2 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: EventCard BindMonster2
bindMonster2 = event BindMonster2 Cards.bindMonster2

ability :: Target -> EventAttrs -> Ability
ability target attrs = (mkAbility (toSource attrs) 1 (ReactionAbility Free))
  { abilityMetadata = Just (TargetMetadata target)
  }

instance HasActions BindMonster2 where
  getActions (BindMonster2 attrs@EventAttrs {..}) = restricte
  getActions iid (WhenWouldReady target) (BindMonster2 attrs@EventAttrs {..})
    | iid == eventOwner = pure
      [ ability target attrs | target `elem` eventAttachedTarget ]
  getActions iid window (BindMonster2 attrs) = getActions iid window attrs

instance HasModifiersFor env BindMonster2

instance HasQueue env => RunMessage env BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ CreateEffect "02031" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy iid (EventSource eid) SkillWillpower False
      ]
    SkillTestEnds _ ->
      e <$ when (null eventAttachedTarget) (push (Discard $ toTarget attrs))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
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
