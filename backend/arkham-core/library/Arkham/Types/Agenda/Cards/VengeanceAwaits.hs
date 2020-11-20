{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype VengeanceAwaits = VengeanceAwaits Attrs
  deriving newtype (Show, ToJSON, FromJSON)

vengeanceAwaits :: VengeanceAwaits
vengeanceAwaits = VengeanceAwaits
  $ baseAttrs "01145" 3 "Vengeance Awaits" "Agenda 3a" (Static 5)

instance HasActions env VengeanceAwaits where
  getActions i window (VengeanceAwaits x) = getActions i window x

instance AgendaRunner env => RunMessage env VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@Attrs {..}) = case msg of
    EnemyDefeated _ _ _ "01156" _ _ -> a <$ unshiftMessage (Resolution 2)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3a" -> do
      actIds <- getSetList ()
      if "01146" `elem` actIds
        then
          unshiftMessages
          $ [PlaceLocation "01156", CreateEnemyAt "01157" "01156"]
          <> [ Discard (ActTarget actId) | actId <- actIds ]
        else do
          enemyIds <- getSetList (LocationId "01156")
          unshiftMessages
            $ [ Discard (EnemyTarget eid) | eid <- enemyIds ]
            <> [CreateEnemyAt "01157" "01156"]
            <> [ Discard (ActTarget actId) | actId <- actIds ]
      pure $ VengeanceAwaits $ attrs & sequence .~ "Agenda 3b" & flipped .~ True
    _ -> VengeanceAwaits <$> runMessage msg attrs
