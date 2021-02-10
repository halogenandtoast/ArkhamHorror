module Arkham.Types.Event.Cards.LetMeHandleThis
  ( letMeHandleThis
  , LetMeHandleThis(..)
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

newtype LetMeHandleThis = LetMeHandleThis EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThis :: InvestigatorId -> EventId -> LetMeHandleThis
letMeHandleThis iid uuid = LetMeHandleThis $ baseAttrs iid uuid "03022"

instance HasModifiersFor env LetMeHandleThis where
  getModifiersFor = noModifiersFor

instance HasActions env LetMeHandleThis where
  getActions iid (InHandWindow ownerId (WhenDrawNonPerilTreachery who tid)) (LetMeHandleThis attrs)
    | who /= You && iid == ownerId
    = pure
      [ InitiatePlayCard
          iid
          (getCardId attrs)
          (Just $ TreacheryTarget tid)
          False
      ]
  getActions iid window (LetMeHandleThis attrs) = getActions iid window attrs

instance HasQueue env => RunMessage env LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid (Just (TreacheryTarget tid))
      | eid == eventId -> do
        withQueue_ $ map $ \case
          Revelation _ (TreacherySource tid') | tid == tid' ->
            Revelation iid (TreacherySource tid')
          AfterRevelation _ tid' | tid == tid' -> AfterRevelation iid tid'
          Surge _ (TreacherySource tid') | tid == tid' ->
            Surge iid (TreacherySource tid')
          other -> other
        e <$ unshiftMessages
          [ CreateEffect
            eventCardCode
            Nothing
            (toSource attrs)
            (InvestigatorTarget iid)
          , Discard (toTarget attrs)
          ]
    _ -> LetMeHandleThis <$> runMessage msg attrs
