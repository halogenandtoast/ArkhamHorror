{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Agenda
  ( module Arkham.Agenda
  ) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Id

lookupAgenda :: AgendaId -> (Int -> Agenda)
lookupAgenda agendaId = case lookup (unAgendaId agendaId) allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show agendaId
  Just (SomeAgendaCard a) -> \i -> Agenda $ cbCardBuilder a (i, agendaId)

instance RunMessage Agenda where
  runMessage msg (Agenda a) = Agenda <$> runMessage msg a

instance FromJSON Agenda where
  parseJSON v = flip (withObject "Agenda") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    withAgendaCardCode cCode $ \(_ :: AgendaCard a) -> Agenda <$> parseJSON @a v

withAgendaCardCode
  :: CardCode -> (forall a . IsAgenda a => AgendaCard a -> r) -> r
withAgendaCardCode cCode f = case lookup cCode allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show cCode
  Just (SomeAgendaCard a) -> f a

allAgendas :: HashMap CardCode SomeAgendaCard
allAgendas = mapFromList $ map
  (toFst someAgendaCardCode)
  [ -- Night of the Zealot
  -- The Gathering
    SomeAgendaCard whatsGoingOn
  , SomeAgendaCard riseOfTheGhouls
  , SomeAgendaCard theyreGettingOut
  ]
