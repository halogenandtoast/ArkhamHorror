{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.CampusSafety where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype CampusSafety = CampusSafety Attrs
  deriving newtype (Show, ToJSON, FromJSON)

campusSafety :: CampusSafety
campusSafety = CampusSafety $ baseAttrs "02047" "CampusSafety" (Act 3 A)

instance HasActions env CampusSafety where
  getActions i window (CampusSafety x) = getActions i window x

instance ActRunner env => RunMessage env CampusSafety where
  runMessage msg (CampusSafety attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && not actFlipped -> do
      unshiftMessage (AdvanceAct aid)
      pure $ CampusSafety $ attrs & sequenceL .~ Act 3 B & flippedL .~ True
    AdvanceAct aid | aid == actId && actFlipped -> do
      alchemyLabsInPlay <- elem (LocationName "Alchemy Labs") <$> getList ()
      agendaStep <- asks $ unAgendaStep . getStep
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      unshiftMessages
        $ [ PlaceLocationNamed "Alchemy Labs" | not alchemyLabsInPlay ]
        <> [ CreateEnemyAtLocationNamed "02058" (LocationName "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationNamed
               "02059"
               (LocationName "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure $ CampusSafety $ attrs & sequenceL .~ Act 3 B & flippedL .~ True
    PrePlayerWindow -> do
      totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      pure
        $ CampusSafety
        $ attrs
        & canAdvanceL
        .~ (totalSpendableClues >= requiredClues)
    _ -> CampusSafety <$> runMessage msg attrs
