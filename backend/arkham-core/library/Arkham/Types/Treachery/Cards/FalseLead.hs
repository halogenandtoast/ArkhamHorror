module Arkham.Types.Treachery.Cards.FalseLead where

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

newtype FalseLead = FalseLead TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLead :: TreacheryId -> a -> FalseLead
falseLead uuid _ = FalseLead $ baseAttrs uuid "01136"

instance HasModifiersFor env FalseLead where
  getModifiersFor = noModifiersFor

instance HasActions env FalseLead where
  getActions i window (FalseLead attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env FalseLead where
  runMessage msg t@(FalseLead attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerClueCount <- unClueCount <$> getCount iid
      if playerClueCount == 0
        then t <$ unshiftMessages
          [chooseOne iid [Surge iid (toSource attrs)], Discard $ toTarget attrs]
        else t <$ unshiftMessages
          [ RevelationSkillTest iid source SkillIntellect 4
          , Discard $ toTarget attrs
          ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
      | tid == treacheryId -> t
      <$ unshiftMessage (InvestigatorPlaceCluesOnLocation iid n)
    _ -> FalseLead <$> runMessage msg attrs
