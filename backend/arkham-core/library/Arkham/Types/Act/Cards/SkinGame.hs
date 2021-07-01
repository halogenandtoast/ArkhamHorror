module Arkham.Types.Act.Cards.SkinGame
  ( SkinGame(..)
  , skinGame
  ) where

import Arkham.Prelude

import Arkham.PlayerCard
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.ScenarioId
import Arkham.Types.Trait

newtype SkinGame = SkinGame ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

skinGame :: SkinGame
skinGame = SkinGame $ baseAttrs
  "02067"
  "Skin Game"
  (Act 2 A)
  (Just $ RequiredClues (PerPlayer 2) (Just $ LocationWithTitle "VIP Area"))

instance ActionRunner env => HasActions env SkinGame where
  getActions i window (SkinGame x) = getActions i window x

instance ActRunner env => RunMessage env SkinGame where
  runMessage msg a@(SkinGame attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      vipAreaId <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "VIP Area"
      investigatorIds <- getSetList vipAreaId
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ chooseOne iid [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ SkinGame $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      peterClover <- PlayerCard <$> genPlayerCard "02079"
      drFrancisMorgan <- PlayerCard <$> genPlayerCard "02080"
      cloverClubBarId <- getJustLocationIdByName "Clover Club Bar"
      vipAreaId <- getJustLocationIdByName "VIP Area"
      a <$ if completedExtracurricularActivity
        then unshiftMessages
          [ CreateStoryAssetAt peterClover cloverClubBarId
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardMatchByType (EnemyType, singleton Abomination))
          , NextAct actId "02068"
          ]
        else unshiftMessages
          [CreateStoryAssetAt drFrancisMorgan vipAreaId, NextAct actId "02068"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      cloverClubBarId <- getJustLocationIdByName "Clover Club Bar"
      a <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) cloverClubBarId)
    _ -> SkinGame <$> runMessage msg attrs
