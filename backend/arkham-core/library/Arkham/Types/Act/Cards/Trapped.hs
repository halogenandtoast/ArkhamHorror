{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.Trapped where

import Arkham.Import

import Arkham.Types.Act.Attrs
import qualified Arkham.Types.Act.Attrs as Act
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype Trapped = Trapped Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trapped :: Trapped
trapped = Trapped $ baseAttrs "01108" "Trapped" "Act 1a"

instance HasActions env Trapped where
  getActions i window (Trapped x) = getActions i window x

instance ActRunner env => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        [ SpendClues requiredClues investigatorIds
        , chooseOne leadInvestigatorId [AdvanceAct aid]
        ]
      pure $ Trapped $ attrs & Act.sequence .~ "Act 1b" & flipped .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 1b" -> do
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
    PrePlayerWindow -> do
      totalSpendableClues <- getSpendableClueCount =<< getInvestigatorIds
      requiredClues <- getPlayerCountValue (PerPlayer 2)
      pure
        $ Trapped
        $ attrs
        & canAdvance
        .~ (totalSpendableClues >= requiredClues)
    _ -> Trapped <$> runMessage msg attrs
