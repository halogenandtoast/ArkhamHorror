{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.Trapped where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype Trapped = Trapped Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trapped :: Trapped
trapped = Trapped $ baseAttrs "01108" "Trapped" "Act 1a"

instance (ActRunner env) => RunMessage env Trapped where
  runMessage msg a@(Trapped attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 1a" -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      playerCount <- unPlayerCount <$> asks (getCount ())
      unshiftMessages
        (SpendClues (fromGameValue (PerPlayer 2) playerCount) investigatorIds
        : [ Ask iid $ ChooseOne [AdvanceAct aid] | iid <- investigatorIds ]
        )
      pure $ Trapped $ attrs & sequence .~ "Act 1b" & flipped .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 1b" -> do
      enemyIds <- HashSet.toList <$> asks (getSet (LocationId "01111"))
      a <$ unshiftMessages
        ([ PlaceLocation "01112"
         , PlaceLocation "01114"
         , PlaceLocation "01113"
         , PlaceLocation "01115"
         ]
        <> map (Discard . EnemyTarget) enemyIds
        <> [ RevealLocation "01112"
           , MoveAllTo "01112"
           , RemoveLocation "01111"
           , NextAct aid "01109"
           ]
        )
    PrePlayerWindow -> do
      clueCount <- unClueCount <$> asks (getCount AllInvestigators)
      playerCount <- unPlayerCount <$> asks (getCount ())
      pure
        $ Trapped
        $ attrs
        & canAdvance
        .~ (clueCount >= fromGameValue (PerPlayer 2) playerCount)
    _ -> Trapped <$> runMessage msg attrs
