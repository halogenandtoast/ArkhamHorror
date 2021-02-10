module Arkham.Types.Agenda.Cards.QuietHalls where

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


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Game.Helpers
import Control.Monad.Extra (mapMaybeM)

newtype QuietHalls = QuietHalls AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls :: QuietHalls
quietHalls =
  QuietHalls $ baseAttrs "02042" "Quiet Halls" (Agenda 1 A) (Static 7)

instance HasModifiersFor env QuietHalls where
  getModifiersFor = noModifiersFor

instance HasActions env QuietHalls where
  getActions i window (QuietHalls x) = getActions i window x

instance AgendaRunner env => RunMessage env QuietHalls where
  runMessage msg a@(QuietHalls attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
      investigatorIds <- getInvestigatorIds
      messages <- flip mapMaybeM investigatorIds $ \iid -> do
        discardCount <- unDiscardCount <$> getCount iid
        if discardCount >= 5
          then pure $ Just
            (InvestigatorAssignDamage
              iid
              (toSource attrs)
              DamageAny
              0
              (if discardCount >= 10 then 2 else 1)
            )
          else pure Nothing

      unshiftMessages messages

      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()

      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      let
        continueMessages = if completedTheHouseAlwaysWins
          then [NextAgenda aid "02043", AdvanceCurrentAgenda]
          else [NextAgenda aid "02043"]

      a <$ unshiftMessage
        (chooseOne leadInvestigatorId [Label "Continue" continueMessages])
    _ -> QuietHalls <$> runMessage msg attrs
