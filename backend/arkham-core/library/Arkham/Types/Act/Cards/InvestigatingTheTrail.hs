module Arkham.Types.Act.Cards.InvestigatingTheTrail where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype InvestigatingTheTrail = InvestigatingTheTrail ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

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
      mRitualSiteId <- getLocationIdByName "Ritual Site"
      mainPathId <- getJustLocationIdByName "Main Path"
      when (isNothing mRitualSiteId) $ do
        ritualSiteId <- getRandom
        unshiftMessage (PlaceLocation "01156" ritualSiteId)
      cultistsWhoGotAwayCardCodes <- hasRecordSet CultistsWhoGotAway
      cultistsWhoGotAway <- for cultistsWhoGotAwayCardCodes genEncounterCard
      a <$ unshiftMessages
        ([ CreateEnemyAt (EncounterCard card) mainPathId Nothing
         | card <- cultistsWhoGotAway
         ]
        <> [NextAct aid "01147"]
        )
    _ -> InvestigatingTheTrail <$> runMessage msg attrs
