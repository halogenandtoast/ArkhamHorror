{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Agenda (module Arkham.Agenda) where

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Prelude

lookupAgenda :: AgendaId -> Int -> CardId -> Agenda
lookupAgenda agendaId = case lookup (unAgendaId agendaId) allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show agendaId
  Just (SomeAgendaCard a) -> \i cardId -> Agenda $ cbCardBuilder a cardId (i, agendaId)

instance RunMessage Agenda where
  runMessage msg (Agenda a) = Agenda <$> runMessage msg a

instance FromJSON Agenda where
  parseJSON = withObject "Agenda" $ \o -> do
    cCode <- o .: "id"
    withAgendaCardCode cCode
      $ \(_ :: AgendaCard a) -> Agenda <$> parseJSON @a (Object o)

withAgendaCardCode
  :: CardCode -> (forall a. IsAgenda a => AgendaCard a -> r) -> r
withAgendaCardCode cCode f = case lookup cCode allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show cCode
  Just (SomeAgendaCard a) -> f a

allAgendas :: Map CardCode SomeAgendaCard
allAgendas =
  (mapFrom someAgendaCardCode Registry.agendas <>)
    $ mapFrom someAgendaCardCode allAgendaCardBuilders
