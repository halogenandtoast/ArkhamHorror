{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.InvestigatingTheTrail where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey

newtype InvestigatingTheTrail = InvestigatingTheTrail Attrs
  deriving newtype (Show, ToJSON, FromJSON)

investigatingTheTrail :: InvestigatingTheTrail
investigatingTheTrail =
  InvestigatingTheTrail $ baseAttrs "01146" "Investigating the Trail" (Act 1 A)

instance HasActions env InvestigatingTheTrail where
  getActions i window (InvestigatingTheTrail x) = getActions i window x

instance ActRunner env => RunMessage env InvestigatingTheTrail where
  runMessage msg a@(InvestigatingTheTrail attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == Act 1 A -> do
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        (SpendClues requiredClues investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure
        $ InvestigatingTheTrail
        $ attrs
        & (sequenceL .~ Act 1 B)
        & (flippedL .~ True)
    AdvanceAct aid | aid == actId && actSequence == Act 1 B -> do
      locationIds <- setToList <$> getLocationSet
      when ("01156" `notElem` locationIds)
        $ unshiftMessage (PlaceLocation "01156")
      cultistsWhoGotAway <- asks (hasRecordSet CultistsWhoGotAway)
      a <$ unshiftMessages
        ([ CreateEnemyAt cardCode "01149" | cardCode <- cultistsWhoGotAway ]
        <> [NextAct aid "01147"]
        )
    PrePlayerWindow -> do
      totalSpendableClueCount <- getSpendableClueCount =<< getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      pure
        $ InvestigatingTheTrail
        $ attrs
        & (canAdvanceL .~ (totalSpendableClueCount >= requiredClues))
    _ -> InvestigatingTheTrail <$> runMessage msg attrs
