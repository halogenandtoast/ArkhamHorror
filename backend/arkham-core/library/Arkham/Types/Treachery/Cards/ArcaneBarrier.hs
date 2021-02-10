module Arkham.Types.Treachery.Cards.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
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


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryId -> a -> ArcaneBarrier
arcaneBarrier uuid _ = ArcaneBarrier $ baseAttrs uuid "02102"

instance HasModifiersFor env ArcaneBarrier where
  getModifiersFor = noModifiersFor

instance HasActions env ArcaneBarrier where
  getActions i window (ArcaneBarrier attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      t <$ unshiftMessage (AttachTreachery (toId attrs) (LocationTarget lid))
    Will (MoveTo iid lid) -> do
      investigatorLocation <- getId iid
      when
          (treacheryOnLocation lid attrs
          || treacheryOnLocation investigatorLocation attrs
          )
        $ do
            moveFromMessage <- fromJustNote "missing move from" <$> popMessage
            moveToMessage <- fromJustNote "missing move to" <$> popMessage
            unshiftMessage
              (CreateEffect
                (CardCode "02102")
                (Just (EffectMessages [moveFromMessage, moveToMessage]))
                (toSource attrs)
                (InvestigatorTarget iid)
              )
      pure t
    _ -> ArcaneBarrier <$> runMessage msg attrs
