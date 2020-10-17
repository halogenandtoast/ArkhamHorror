{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.TheBarrier where

import Arkham.Import

import Arkham.Types.Act.Attrs
import qualified Arkham.Types.Act.Attrs as Act
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype TheBarrier = TheBarrier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theBarrier :: TheBarrier
theBarrier = TheBarrier $ baseAttrs "01109" "The Barrier" "Act 2a"

instance HasActions env TheBarrier where
  getActions i window (TheBarrier x) = getActions i window x

instance (ActRunner env) => RunMessage env TheBarrier where
  runMessage msg a@(TheBarrier attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 2a" -> do
      investigatorIds <- asks (setToList . getSet (LocationId "01112"))
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure $ TheBarrier $ attrs & Act.sequence .~ "Act 2b" & flipped .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 2b" ->
      a <$ unshiftMessages
        [ RevealLocation "01115"
        , CreateStoryAssetAt "01117" "01115"
        , CreateEnemyAt "01116" "01112"
        , NextAct aid "01110"
        ]
    EndRoundWindow -> do
      investigatorIds <- asks
        (setToList . getSet @InvestigatorId (LocationId "01112"))
      leadInvestigatorId <- getLeadInvestigatorId
      totalSpendableClueCount <- getSpendableClueCount investigatorIds
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      if totalSpendableClueCount >= requiredClueCount
        then a <$ unshiftMessage
          (chooseOne
            leadInvestigatorId
            [AdvanceAct actId, Continue "Continue without advancing act"]
          )
        else pure a
    _ -> TheBarrier <$> runMessage msg attrs
