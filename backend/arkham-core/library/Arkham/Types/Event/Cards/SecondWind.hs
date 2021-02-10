module Arkham.Types.Event.Cards.SecondWind
  ( secondWind
  , SecondWind(..)
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

newtype SecondWind = SecondWind EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondWind :: InvestigatorId -> EventId -> SecondWind
secondWind iid uuid = SecondWind $ baseAttrs iid uuid "04149"

instance HasCount ActionTakenCount env InvestigatorId => HasActions env SecondWind where
  getActions iid (InHandWindow ownerId (DuringTurn You)) (SecondWind attrs)
    | iid == ownerId = do
      actionsTaken <- unActionTakenCount <$> getCount iid
      pure
        [ InitiatePlayCard iid (getCardId attrs) Nothing True
        | actionsTaken == 0
        ]
  getActions iid window (SecondWind attrs) = getActions iid window attrs

instance HasModifiersFor env SecondWind where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasRoundHistory env) => RunMessage env SecondWind where
  runMessage msg e@(SecondWind attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      roundHistory <- getRoundHistory =<< ask
      let
        didDrawTreachery = \case
          DrewTreachery iid' _ -> iid == iid'
          _ -> False
        damageToHeal = if any didDrawTreachery roundHistory then 2 else 1
      e <$ unshiftMessages
        [ HealDamage (InvestigatorTarget iid) damageToHeal
        , DrawCards iid 1 False
        , Discard (toTarget attrs)
        ]
    _ -> SecondWind <$> runMessage msg attrs
