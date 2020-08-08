{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda
  ( Agenda(..)
  , lookupAgenda
  )
where

import Arkham.Json
import Arkham.Types.AgendaId
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Agenda.Cards.TheyreGettingOut
import Arkham.Types.Agenda.Cards.RiseOfTheGhouls
import Arkham.Types.Agenda.Cards.WhatsGoingOn
import Arkham.Types.Classes
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)
import ClassyPrelude

lookupAgenda :: AgendaId -> Agenda
lookupAgenda = fromJustNote "Unknown agenda" . flip HashMap.lookup allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = HashMap.fromList $ map
  (\a -> (agendaId $ agendaAttrs a, a))
  [WhatsGoingOn' whatsGoingOn, RiseOfTheGhouls' riseOfTheGhouls, TheyreGettingOut' theyreGettingOut]

instance HasAbilities Agenda where
  getAbilities = agendaAbilities . agendaAttrs

data Agenda
  = WhatsGoingOn' WhatsGoingOn
  | RiseOfTheGhouls' RiseOfTheGhouls
  | TheyreGettingOut' TheyreGettingOut
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

agendaAttrs :: Agenda -> Attrs
agendaAttrs = \case
  WhatsGoingOn' attrs -> coerce attrs
  RiseOfTheGhouls' attrs -> coerce attrs
  TheyreGettingOut' attrs -> coerce attrs

instance (AgendaRunner env) => RunMessage env Agenda where
  runMessage msg = \case
    WhatsGoingOn' x -> WhatsGoingOn' <$> runMessage msg x
    RiseOfTheGhouls' x -> RiseOfTheGhouls' <$> runMessage msg x
    TheyreGettingOut' x -> TheyreGettingOut' <$> runMessage msg x
