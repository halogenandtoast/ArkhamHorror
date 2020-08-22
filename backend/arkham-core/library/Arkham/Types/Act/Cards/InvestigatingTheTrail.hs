{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.InvestigatingTheTrail where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype InvestigatingTheTrail = InvestigatingTheTrail Attrs
  deriving newtype (Show, ToJSON, FromJSON)

investigatingTheTrail :: InvestigatingTheTrail
investigatingTheTrail =
  InvestigatingTheTrail $ baseAttrs "01146" "Investigating the Trail" "Act 1a"

instance HasActions env investigator InvestigatingTheTrail where
  getActions i window (InvestigatingTheTrail x) = getActions i window x

instance (ActRunner env) => RunMessage env InvestigatingTheTrail where
  runMessage msg (InvestigatingTheTrail attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> error "TODO"
    PrePlayerWindow -> do
      totalSpendableClueCount <- unSpendableClueCount
        <$> asks (getCount AllInvestigators)
      playerCount <- unPlayerCount <$> asks (getCount ())
      pure
        $ InvestigatingTheTrail
        $ attrs
        & canAdvance
        .~ (totalSpendableClueCount >= fromGameValue (PerPlayer 3) playerCount)
    _ -> InvestigatingTheTrail <$> runMessage msg attrs
