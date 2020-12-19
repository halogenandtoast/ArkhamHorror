{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.RicesWhereabouts where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype RicesWhereabouts = RicesWhereabouts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ricesWhereabouts :: RicesWhereabouts
ricesWhereabouts =
  RicesWhereabouts $ baseAttrs "02046" "Rice's Whereabouts" "Act 2a"

instance HasActions env RicesWhereabouts where
  getActions i window (RicesWhereabouts x) = getActions i window x

instance ActRunner env => RunMessage env RicesWhereabouts where
  runMessage msg (RicesWhereabouts attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && not actFlipped -> do
      unshiftMessage (AdvanceAct aid)
      pure . RicesWhereabouts $ attrs & sequenceL .~ "Act 2b" & flippedL .~ True
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
      pure
        $ RicesWhereabouts
        $ attrs
        & (sequenceL .~ "Act 1b")
        & (flippedL .~ True)
    PrePlayerWindow -> do
      totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 3)
      pure
        $ RicesWhereabouts
        $ attrs
        & (canAdvanceL .~ (totalSpendableClues >= requiredClues))
    _ -> RicesWhereabouts <$> runMessage msg attrs
