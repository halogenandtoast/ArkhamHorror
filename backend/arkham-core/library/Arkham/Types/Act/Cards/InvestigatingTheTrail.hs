{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.InvestigatingTheTrail where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype InvestigatingTheTrail = InvestigatingTheTrail Attrs
  deriving newtype (Show, ToJSON, FromJSON)

investigatingTheTrail :: InvestigatingTheTrail
investigatingTheTrail =
  InvestigatingTheTrail $ baseAttrs "01146" "Investigating the Trail" "Act 1a"

instance HasActions env investigator InvestigatingTheTrail where
  getActions i window (InvestigatingTheTrail x) = getActions i window x

instance (ActRunner env) => RunMessage env InvestigatingTheTrail where
  runMessage msg a@(InvestigatingTheTrail attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      playerCount <- unPlayerCount <$> asks (getCount ())
      unshiftMessages
        (SpendClues (fromGameValue (PerPlayer 3) playerCount) investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure
        $ InvestigatingTheTrail
        $ attrs
        & sequence
        .~ "Act 1b"
        & flipped
        .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 1b" -> do
      locationIds <- HashSet.toList <$> asks (getSet @LocationId ())
      when ("01156" `notElem` locationIds)
        $ unshiftMessage (PlaceLocation "01156")
      cultistsWhoGotAway <- asks (hasRecordSet CultistsWhoGotAway)
      a <$ unshiftMessages
        ([ CreateEnemyAt cardCode "01149" | cardCode <- cultistsWhoGotAway ]
        <> [NextAct aid "01147"]
        )
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
