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

lookupAgenda :: AgendaId -> Agenda
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId)
    $ lookup agendaId allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = mapFromList $ map
  (toFst $ agendaId . toAttrs)
  [ WhatsGoingOn' whatsGoingOn
  , RiseOfTheGhouls' riseOfTheGhouls
  , TheyreGettingOut' theyreGettingOut
  , PredatorOrPrey' predatorOrPrey
  , TimeIsRunningShort' timeIsRunningShort
  , TheArkhamWoods' theArkhamWoods
  , TheRitualBegins' theRitualBegins
  , VengeanceAwaits' vengeanceAwaits
  , QuietHalls' quietHalls
  , DeadOfNight' deadOfNight
  , TheBeastUnleashed' theBeastUnleashed
  , ReturnToPredatorOrPrey' returnToPredatorOrPrey
  , ACreatureOfTheBayou' aCreatureOfTheBayou
  , TheRougarouFeeds' theRougarouFeeds
  , TheCurseSpreads' theCurseSpreads
  ]

instance HasAbilities Agenda where
  getAbilities = agendaAbilities . toAttrs

instance HasCount DoomCount env Agenda where
  getCount = pure . DoomCount . agendaDoom . toAttrs

getAgendaId :: Agenda -> AgendaId
getAgendaId = agendaId . toAttrs

instance HasId AgendaId env Agenda where
  getId = pure . getAgendaId

instance HasStep AgendaStep Agenda where
  getStep = AgendaStep . agendaNumber . toAttrs

data Agenda
  = WhatsGoingOn' WhatsGoingOn
  | RiseOfTheGhouls' RiseOfTheGhouls
  | TheyreGettingOut' TheyreGettingOut
  | PredatorOrPrey' PredatorOrPrey
  | TimeIsRunningShort' TimeIsRunningShort
  | TheArkhamWoods' TheArkhamWoods
  | TheRitualBegins' TheRitualBegins
  | VengeanceAwaits' VengeanceAwaits
  | QuietHalls' QuietHalls
  | DeadOfNight' DeadOfNight
  | TheBeastUnleashed' TheBeastUnleashed
  | ReturnToPredatorOrPrey' ReturnToPredatorOrPrey
  | ACreatureOfTheBayou' ACreatureOfTheBayou
  | TheRougarouFeeds' TheRougarouFeeds
  | TheCurseSpreads' TheCurseSpreads
  | BaseAgenda' BaseAgenda
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Agenda
deriving anyclass instance (AgendaRunner env) => RunMessage env Agenda
deriving anyclass instance HasModifiersFor env Agenda

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  toId = toId . toAttrs
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

newtype BaseAgenda = BaseAgenda Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseAgenda
  :: AgendaId -> Text -> Text -> GameValue Int -> (Attrs -> Attrs) -> Agenda
baseAgenda a b c d f = BaseAgenda' . BaseAgenda . f $ baseAttrs a 1 b c d

instance HasModifiersFor env BaseAgenda where
  getModifiersFor = noModifiersFor

instance HasActions env BaseAgenda where
  getActions iid window (BaseAgenda attrs) = getActions iid window attrs

instance AgendaRunner env => RunMessage env BaseAgenda where
  runMessage msg (BaseAgenda attrs) = BaseAgenda <$> runMessage msg attrs

instance HasAttrs Agenda where
  type AttrsT Agenda = Attrs
  toAttrs = toAttrs . toAttrs
