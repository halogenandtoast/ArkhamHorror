module Arkham.Types.Event.Cards.BindMonster2
  ( bindMonster2
  , BindMonster2(..)
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Event.Attrs

newtype BindMonster2 = BindMonster2 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindMonster2 :: InvestigatorId -> EventId -> BindMonster2
bindMonster2 iid uuid = BindMonster2 $ baseAttrs iid uuid "02031"

ability :: Target -> EventAttrs -> Ability
ability target attrs = (mkAbility (toSource attrs) 1 (ReactionAbility Free))
  { abilityMetadata = Just (TargetMetadata target)
  }

instance HasActions env BindMonster2 where
  getActions iid (WhenWouldReady target) (BindMonster2 attrs@EventAttrs {..})
    | iid == eventOwner = pure
      [ ActivateCardAbilityAction eventOwner (ability target attrs)
      | target `elem` eventAttachedTarget
      ]
  getActions iid window (BindMonster2 attrs) = getActions iid window attrs

instance HasModifiersFor env BindMonster2 where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BindMonster2 where
  runMessage msg e@(BindMonster2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ CreateEffect "02031" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy iid (EventSource eid) SkillWillpower False
      ]
    SkillTestEnds _ -> e <$ when
      (null eventAttachedTarget)
      (unshiftMessage (Discard $ toTarget attrs))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      case eventAttachedTarget of
        Just target -> e <$ unshiftMessage
          (BeginSkillTest iid source target Nothing SkillWillpower 3)
        Nothing -> throwIO $ InvalidState "must be attached"
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> case eventAttachedTarget of
        Just target@(EnemyTarget _) ->
          e <$ withQueue_ (filter (/= Ready target))
        _ -> error "invalid target"
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> e <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> BindMonster2 <$> runMessage msg attrs
