module Arkham.Types.Act.Cards.SkinGame
  ( SkinGame(..)
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
skinGame = SkinGame $ baseAttrs
  "02067"
  "Skin Game"
  (Act 2 A)
  (Just $ RequiredClues (PerPlayer 2) (Just $ LocationWithTitle "VIP Area"))

instance ActionRunner env => HasActions env SkinGame where
  getActions i window (SkinGame x) = getActions i window x

instance ActRunner env => RunMessage env SkinGame where
  runMessage msg a@(SkinGame attrs@Attrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      vipAreaId <- fromJustNote "must exist"
        <$> getId @(Maybe LocationId) (LocationWithTitle "VIP Area")
      investigatorIds <- getSetList vipAreaId
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ SkinGame $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
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
    _ -> SkinGame <$> runMessage msg attrs
