module Arkham.Types.Act.Cards.InvestigatingTheTrail where


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
