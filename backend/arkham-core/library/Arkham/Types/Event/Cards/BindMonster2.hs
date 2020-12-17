{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Event.Cards.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers

newtype BindMonster2 = BindMonster2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bindMonster2 :: InvestigatorId -> EventId -> BindMonster2
bindMonster2 iid uuid = BindMonster2 $ baseAttrs iid uuid "02031"

instance HasActions env BindMonster2 where
  getActions iid window (BindMonster2 attrs) = getActions iid window attrs

instance HasModifiersFor env BindMonster2 where
  getModifiersFor _ target (BindMonster2 attrs@Attrs {..})
    | target `elem` eventAttachedTarget = pure
    $ toModifiers attrs [AlternativeReady (toSource attrs)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@Attrs {..}) = case msg of
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
    _ -> BindMonster2 <$> runMessage msg attrs
