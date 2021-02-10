module Arkham.Types.Act.Cards.InvestigatingTheTrail where

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


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey

newtype InvestigatingTheTrail = InvestigatingTheTrail ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

investigatingTheTrail :: InvestigatingTheTrail
investigatingTheTrail = InvestigatingTheTrail $ baseAttrs
  "01146"
  "Investigating the Trail"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 3) Nothing)

instance ActionRunner env => HasActions env InvestigatingTheTrail where
  getActions i window (InvestigatingTheTrail x) = getActions i window x

instance ActRunner env => RunMessage env InvestigatingTheTrail where
  runMessage msg a@(InvestigatingTheTrail attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        (SpendClues requiredClues investigatorIds
        : [ chooseOne iid [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ InvestigatingTheTrail $ attrs & (sequenceL .~ Act 1 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      locationIds <- setToList <$> getLocationSet
      when ("01156" `notElem` locationIds)
        $ unshiftMessage (PlaceLocation "01156")
      cultistsWhoGotAwayCardCodes <- asks (hasRecordSet CultistsWhoGotAway)
      cultistsWhoGotAway <- for cultistsWhoGotAwayCardCodes genEncounterCard
      a <$ unshiftMessages
        ([ CreateEnemyAt (EncounterCard card) "01149"
         | card <- cultistsWhoGotAway
         ]
        <> [NextAct aid "01147"]
        )
    _ -> InvestigatingTheTrail <$> runMessage msg attrs
