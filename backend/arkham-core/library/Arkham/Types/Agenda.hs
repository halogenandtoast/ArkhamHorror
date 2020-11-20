{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda
  ( Agenda(..)
  , lookupAgenda
  , baseAgenda
  , getAgendaId
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Cards
import Arkham.Types.Agenda.Runner
import Data.Coerce

lookupAgenda :: AgendaId -> Agenda
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId)
    $ lookup agendaId allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = mapFromList $ map
  (toFst $ agendaId . agendaAttrs)
  [ WhatsGoingOn' whatsGoingOn
  , RiseOfTheGhouls' riseOfTheGhouls
  , TheyreGettingOut' theyreGettingOut
  , PredatorOrPrey' predatorOrPrey
  , TimeIsRunningShort' timeIsRunningShort
  , TheArkhamWoods' theArkhamWoods
  , TheRitualBegins' theRitualBegins
  , VengeanceAwaits' vengeanceAwaits
  , ReturnToPredatorOrPrey' returnToPredatorOrPrey
  , ACreatureOfTheBayou' aCreatureOfTheBayou
  , TheRougarouFeeds' theRougarouFeeds
  , TheCurseSpreads' theCurseSpreads
  ]

instance HasAbilities Agenda where
  getAbilities = agendaAbilities . agendaAttrs

instance HasCount DoomCount env Agenda where
  getCount = pure . DoomCount . agendaDoom . agendaAttrs

getAgendaId :: Agenda -> AgendaId
getAgendaId = agendaId . agendaAttrs

instance HasId AgendaId env Agenda where
  getId = pure . getAgendaId

instance HasStep AgendaStep Agenda where
  getStep = AgendaStep . agendaNumber . agendaAttrs

data Agenda
  = WhatsGoingOn' WhatsGoingOn
  | RiseOfTheGhouls' RiseOfTheGhouls
  | TheyreGettingOut' TheyreGettingOut
  | PredatorOrPrey' PredatorOrPrey
  | TimeIsRunningShort' TimeIsRunningShort
  | TheArkhamWoods' TheArkhamWoods
  | TheRitualBegins' TheRitualBegins
  | VengeanceAwaits' VengeanceAwaits
  | ReturnToPredatorOrPrey' ReturnToPredatorOrPrey
  | ACreatureOfTheBayou' ACreatureOfTheBayou
  | TheRougarouFeeds' TheRougarouFeeds
  | TheCurseSpreads' TheCurseSpreads
  | BaseAgenda' BaseAgenda
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Agenda
deriving anyclass instance (AgendaRunner env) => RunMessage env Agenda

instance Entity Agenda where
  toTarget = toTarget . agendaAttrs
  isTarget = isTarget . agendaAttrs
  toSource = toSource . agendaAttrs
  isSource = isSource . agendaAttrs

newtype BaseAgenda = BaseAgenda Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseAgenda
  :: AgendaId -> Text -> Text -> GameValue Int -> (Attrs -> Attrs) -> Agenda
baseAgenda a b c d f = BaseAgenda' . BaseAgenda . f $ baseAttrs a 1 b c d

instance HasActions env BaseAgenda where
  getActions iid window (BaseAgenda attrs) = getActions iid window attrs

instance AgendaRunner env => RunMessage env BaseAgenda where
  runMessage msg (BaseAgenda attrs) = BaseAgenda <$> runMessage msg attrs

agendaAttrs :: Agenda -> Attrs
agendaAttrs = \case
  WhatsGoingOn' attrs -> coerce attrs
  RiseOfTheGhouls' attrs -> coerce attrs
  TheyreGettingOut' attrs -> coerce attrs
  PredatorOrPrey' attrs -> coerce attrs
  TimeIsRunningShort' attrs -> coerce attrs
  TheArkhamWoods' attrs -> coerce attrs
  TheRitualBegins' attrs -> coerce attrs
  VengeanceAwaits' attrs -> coerce attrs
  ReturnToPredatorOrPrey' attrs -> coerce attrs
  ACreatureOfTheBayou' attrs -> coerce attrs
  TheRougarouFeeds' attrs -> coerce attrs
  TheCurseSpreads' attrs -> coerce attrs
  BaseAgenda' attrs -> coerce attrs
