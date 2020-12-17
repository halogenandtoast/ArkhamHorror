{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.Trapped where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype Trapped = Trapped Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trapped :: Trapped
trapped = Trapped $ baseAttrs
  "01108"
  "Trapped"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 2) Nothing)

instance ActionRunner env => HasActions env Trapped where
  getActions i window (Trapped x) = getActions i window x

instance ActRunner env => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@Attrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && not actFlipped -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid $ toSource attrs]
        ]
      pure $ Trapped $ attrs & sequenceL .~ Act 1 B & flippedL .~ True
    AdvanceAct aid _ | aid == actId && actFlipped -> do
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
    _ -> Trapped <$> runMessage msg attrs
