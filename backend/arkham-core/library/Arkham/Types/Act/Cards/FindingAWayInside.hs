{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Act.Cards.FindingAWayInside
  ( FindingAWayInside(..)
  , findingAWayInside
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype FindingAWayInside = FindingAWayInside Attrs
  deriving newtype (Show, ToJSON, FromJSON)

findingAWayInside :: FindingAWayInside
findingAWayInside = FindingAWayInside $ baseAttrs
  "02122"
  "Finding A Way Inside"
  (Act 1 A)
  (Just $ RequiredClues (Static 2) Nothing)

instance ActionRunner env => HasActions env FindingAWayInside where
  getActions i window (FindingAWayInside x) = getActions i window x

instance ActRunner env => RunMessage env FindingAWayInside where
  runMessage msg a@(FindingAWayInside attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && not actFlipped -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      unshiftMessages
        [ SpendClues 2 investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid]
        ]
      pure $ FindingAWayInside $ attrs & sequenceL .~ Act 1 B & flippedL .~ True
    AdvanceAct aid | aid == actId && actFlipped -> do
      enemyIds <- getSetList (LocationId "01111")
      a <$ unshiftMessages
        ([ PlaceLocation "01112"
         , PlaceLocation "01114"
         , PlaceLocation "01113"
         , PlaceLocation "01115"
         ]
        <> map (Discard . EnemyTarget) enemyIds
        <> [ RevealLocation Nothing "01112"
           , MoveAllTo "01112"
           , RemoveLocation "01111"
           , NextAct aid "01109"
           ]
        )
    _ -> FindingAWayInside <$> runMessage msg attrs
