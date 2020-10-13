{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.FindingLadyEsprit where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype FindingLadyEsprit = FindingLadyEsprit Attrs
  deriving newtype (Show, ToJSON, FromJSON)

findingLadyEsprit :: FindingLadyEsprit
findingLadyEsprit =
  FindingLadyEsprit $ baseAttrs "81005" "Finding Lady Esprit" "Act 1a"

instance HasActions env investigator FindingLadyEsprit where
  getActions i window (FindingLadyEsprit x) = getActions i window x

instance (ActRunner env) => RunMessage env FindingLadyEsprit where
  runMessage msg a@(FindingLadyEsprit attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      investigatorIds <- asks $ setToList . getSet ()
      playerCount <- asks $ unPlayerCount . getCount ()
      unshiftMessages
        (SpendClues (fromGameValue (PerPlayer 1) playerCount) investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure $ FindingLadyEsprit $ attrs & sequence .~ "Act 1b" & flipped .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 1b" -> pure a
    PrePlayerWindow -> do
      totalSpendableClueCount <-
        asks $ unSpendableClueCount . getCount AllInvestigators
      playerCount <- asks $ unPlayerCount . getCount ()
      pure
        $ FindingLadyEsprit
        $ attrs
        & canAdvance
        .~ (totalSpendableClueCount >= fromGameValue (PerPlayer 1) playerCount)
    _ -> FindingLadyEsprit <$> runMessage msg attrs
