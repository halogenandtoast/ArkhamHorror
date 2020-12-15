{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.SkinGame
  ( SkinGame
  , skinGame
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait

newtype SkinGame = SkinGame Attrs
  deriving newtype (Show, ToJSON, FromJSON)

skinGame :: SkinGame
skinGame = SkinGame $ baseAttrs "02067" "Skin Game" "Act 2a"

instance HasActions env SkinGame where
  getActions i window (SkinGame x) = getActions i window x

instance ActRunner env => RunMessage env SkinGame where
  runMessage msg a@(SkinGame attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && not actFlipped -> do
      vipAreaId <- fromJustNote "must exist"
        <$> getId @(Maybe LocationId) (LocationName "VIP Area")
      investigatorIds <- getSetList vipAreaId
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure $ SkinGame $ attrs & sequenceL .~ "Act 2b" & flippedL .~ True
    AdvanceAct aid | aid == actId && actFlipped -> do
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      a <$ if completedExtracurricularActivity
        then unshiftMessages
          [ CreateStoryAssetAt "02079" "02072"
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (EncounterCardMatchByType (EnemyType, Just Abomination))
          , NextAct actId "02068"
          ]
        else unshiftMessages
          [CreateStoryAssetAt "02080" "02076", NextAct actId "02068"]
    FoundEncounterCard _ target ec | isTarget attrs target ->
      a <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) "02072")
    PrePlayerWindow -> do
      vipAreaId <- fromJustNote "must exist"
        <$> getId @(Maybe LocationId) (LocationName "VIP Area")
      investigatorIds <- getSetList vipAreaId
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      canAdvance' <- (>= requiredClueCount)
        <$> getSpendableClueCount investigatorIds
      pure $ SkinGame $ attrs & canAdvanceL .~ canAdvance'
    _ -> SkinGame <$> runMessage msg attrs
