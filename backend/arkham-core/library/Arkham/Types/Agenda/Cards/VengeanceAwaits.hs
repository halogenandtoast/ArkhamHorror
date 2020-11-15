{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype VengeanceAwaits = VengeanceAwaits Attrs
  deriving newtype (Show, ToJSON, FromJSON)

vengeanceAwaits :: VengeanceAwaits
vengeanceAwaits = VengeanceAwaits
  $ baseAttrs "01145" 3 "Vengeance Awaits" "Agenda 3a" (Static 5)

instance HasActions env VengeanceAwaits where
  getActions i window (VengeanceAwaits x) = getActions i window x

instance AgendaRunner env => RunMessage env VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@Attrs {..}) = case msg of
    EnemyDefeated _ _ "01156" _ -> a <$ unshiftMessage (Resolution 2)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3a" -> do
      actIds <- HashSet.toList <$> asks (getSet ())
      if "01146" `elem` actIds
        then
          unshiftMessages
          $ [PlaceLocation "01156", CreateEnemyAt "01157" "01156"]
          <> [ Discard (ActTarget actId) | actId <- actIds ]
        else do
          enemyIds <- HashSet.toList <$> asks (getSet (LocationId "01156"))
          unshiftMessages
            $ [ Discard (EnemyTarget eid) | eid <- enemyIds ]
            <> [CreateEnemyAt "01157" "01156"]
            <> [ Discard (ActTarget actId) | actId <- actIds ]
      pure $ VengeanceAwaits $ attrs & sequence .~ "Agenda 3b" & flipped .~ True
    _ -> VengeanceAwaits <$> runMessage msg attrs
