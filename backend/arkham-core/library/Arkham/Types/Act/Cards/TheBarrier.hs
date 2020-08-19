{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.TheBarrier where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype TheBarrier = TheBarrier Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theBarrier :: TheBarrier
theBarrier = TheBarrier $ baseAttrs "01109" "The Barrier" "Act 2a"

instance HasActions env investigator TheBarrier where
  getActions i window (TheBarrier x) = getActions i window x

instance (ActRunner env) => RunMessage env TheBarrier where
  runMessage msg a@(TheBarrier attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 2a" -> do
      investigatorIds <- HashSet.toList <$> asks (getSet (LocationId "01112"))
      playerCount <- unPlayerCount <$> asks (getCount ())
      let requiredClueCount = fromGameValue (PerPlayer 3) playerCount
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure $ TheBarrier $ attrs & sequence .~ "Act 2b" & flipped .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 2b" ->
      a <$ unshiftMessages
        [ RevealLocation "01115"
        , CreateStoryAssetAt "01117" "01115"
        , CreateEnemyAt "01116" "01112"
        , NextAct aid "01110"
        ]
    EndRoundWindow -> do
      investigatorIds <- asks (getSet @InvestigatorId (LocationId "01112"))
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      totalClueCount <- unClueCount . mconcat <$> traverse
        (asks . getCount @ClueCount)
        (HashSet.toList investigatorIds)
      playerCount <- unPlayerCount <$> asks (getCount ())
      let requiredClueCount = fromGameValue (PerPlayer 3) playerCount
      if totalClueCount >= requiredClueCount
        then a <$ unshiftMessage
          (Ask leadInvestigatorId $ ChooseOne
            [AdvanceAct actId, Continue "Continue without advancing act"]
          )
        else pure a
    _ -> TheBarrier <$> runMessage msg attrs
