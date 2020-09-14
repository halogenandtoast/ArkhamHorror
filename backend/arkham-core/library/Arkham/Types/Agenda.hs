{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda
  ( Agenda(..)
  , lookupAgenda
  )
where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Cards.PredatorOrPrey
import Arkham.Types.Agenda.Cards.RiseOfTheGhouls
import Arkham.Types.Agenda.Cards.TheArkhamWoods
import Arkham.Types.Agenda.Cards.TheRitualBegins
import Arkham.Types.Agenda.Cards.TheyreGettingOut
import Arkham.Types.Agenda.Cards.TimeIsRunningShort
import Arkham.Types.Agenda.Cards.VengeanceAwaits
import Arkham.Types.Agenda.Cards.WhatsGoingOn
import Arkham.Types.Agenda.Runner
import Arkham.Types.AgendaId
import Arkham.Types.Classes
import Arkham.Types.Query
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
import Safe (fromJustNote)

lookupAgenda :: AgendaId -> Agenda
lookupAgenda = fromJustNote "Unknown agenda" . flip HashMap.lookup allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = HashMap.fromList $ map
  (\a -> (agendaId $ agendaAttrs a, a))
  [ WhatsGoingOn' whatsGoingOn
  , RiseOfTheGhouls' riseOfTheGhouls
  , TheyreGettingOut' theyreGettingOut
  , PredatorOrPrey' predatorOrPrey
  , TimeIsRunningShort' timeIsRunningShort
  , TheArkhamWoods' theArkhamWoods
  , TheRitualBegins' theRitualBegins
  , VengeanceAwaits' vengeanceAwaits
  ]

instance HasAbilities Agenda where
  getAbilities = agendaAbilities . agendaAttrs

instance HasCount DoomCount () Agenda where
  getCount _ = DoomCount . agendaDoom . agendaAttrs

data Agenda
  = WhatsGoingOn' WhatsGoingOn
  | RiseOfTheGhouls' RiseOfTheGhouls
  | TheyreGettingOut' TheyreGettingOut
  | PredatorOrPrey' PredatorOrPrey
  | TimeIsRunningShort' TimeIsRunningShort
  | TheArkhamWoods' TheArkhamWoods
  | TheRitualBegins' TheRitualBegins
  | VengeanceAwaits' VengeanceAwaits
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Agenda
deriving anyclass instance (AgendaRunner env) => RunMessage env Agenda

agendaAttrs :: Agenda -> Attrs
agendaAttrs = getAttrs

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"
