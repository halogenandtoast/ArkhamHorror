{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Event.Cards.BindMonster
  ( bindMonster
  , BindMonster(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers

newtype BindMonster = BindMonster Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bindMonster :: InvestigatorId -> EventId -> BindMonster
bindMonster iid uuid = BindMonster $ baseAttrs iid uuid "02031"

instance HasActions env BindMonster where
  getActions iid window (BindMonster attrs) = getActions iid window attrs

instance HasModifiersFor env BindMonster where
  getModifiersFor _ target (BindMonster attrs@Attrs {..})
    | target `elem` eventAttachedTarget = pure
    $ toModifiers attrs [AlternativeReady (toSource attrs)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env BindMonster where
  runMessage msg e@(BindMonster attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ CreateEffect "02031" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy iid (EventSource eid) SkillWillpower False
      ]
    SkillTestEnds -> e <$ when
      (null eventAttachedTarget)
      (unshiftMessage (Discard $ toTarget attrs))
    ReadyAlternative source target@(EnemyTarget _) | isSource attrs source ->
      e <$ unshiftMessage
        (BeginSkillTest eventOwner source target Nothing SkillWillpower 3)
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _
      | isSource attrs source -> case eventAttachedTarget of
        Just target@(EnemyTarget _) ->
          e <$ unshiftMessages [Ready target, Discard $ toTarget attrs]
        _ -> error "invalid target"
    _ -> BindMonster <$> runMessage msg attrs
