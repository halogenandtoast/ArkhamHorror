module Arkham.Types.Event.Cards.Teamwork
  ( teamwork
  , Teamwork(..)
  )
where

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
import Arkham.Types.Trait

newtype Teamwork = Teamwork EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teamwork :: InvestigatorId -> EventId -> Teamwork
teamwork iid uuid = Teamwork $ baseAttrs iid uuid "02018"

instance HasActions env Teamwork where
  getActions iid window (Teamwork attrs) = getActions iid window attrs

instance HasModifiersFor env Teamwork where
  getModifiersFor = noModifiersFor

-- | Resolve Teamwork Event
--
-- This event works a little special due to how the interactions work with
-- many players. It is not enough to use ChooseSome because resources can
-- be traded many times. Because of this we introduced the ResolveEvent
-- message which is meant to be an internal message inside events after they
-- have resolved and behavior needs to be handled.

instance
  ( HasQueue env
  , HasSet AssetId env (InvestigatorId, [Trait])
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env Teamwork where
  runMessage msg e@(Teamwork attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid mtarget | eid == eventId ->
      e <$ unshiftMessage (ResolveEvent iid eid mtarget)
    ResolveEvent iid eid mtarget | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      assetsWithInvestigatorIds <- concat <$> for
        investigatorIds
        (\investigatorId ->
          map (investigatorId, ) <$> getSetList (investigatorId, [Ally, Item])
        )
      e <$ unshiftMessage
        (chooseOne
          iid
          (Done
          : [ TargetLabel
                (AssetTarget aid)
                [ BeginTrade
                  iid'
                  (AssetTarget aid)
                  (investigatorIds \\ [iid'])
                , ResolveEvent iid eid mtarget
                ]
            | (iid', aid) <- assetsWithInvestigatorIds
            ]
          <> [ TargetLabel
                 (InvestigatorTarget iid')
                 [BeginTrade iid' ResourceTarget (investigatorIds \\ [iid'])]
             | iid' <- investigatorIds
             ]
          )
        )
    _ -> Teamwork <$> runMessage msg attrs
