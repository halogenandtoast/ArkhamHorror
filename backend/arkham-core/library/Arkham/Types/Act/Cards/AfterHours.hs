{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.AfterHours where

import Arkham.Import

import Arkham.Types.Act.Attrs
import qualified Arkham.Types.Act.Attrs as Act
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype AfterHours = AfterHours Attrs
  deriving newtype (Show, ToJSON, FromJSON)

afterHours :: AfterHours
afterHours = AfterHours $ baseAttrs "02045" "After Hours" "Act 1a"

instance HasActions env AfterHours where
  getActions i window (AfterHours x) = getActions i window x

instance ActRunner env => RunMessage env AfterHours where
  runMessage msg a@(AfterHours attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && not actFlipped -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid]
        ]
      pure $ AfterHours $ attrs & Act.sequence .~ "Act 1b" & flipped .~ True
    AdvanceAct aid | aid == actId && actFlipped -> a <$ unshiftMessages
      [ AddCampaignCardToEncounterDeck "02060"
      , ShuffleEncounterDiscardBackIn
      , NextAct aid "02046"
      ]
    PrePlayerWindow -> do
      totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      pure
        $ AfterHours
        $ attrs
        & canAdvance
        .~ (totalSpendableClues >= requiredClues)
    _ -> AfterHours <$> runMessage msg attrs
