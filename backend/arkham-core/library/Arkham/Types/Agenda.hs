module Arkham.Types.Agenda
  ( Agenda(..)
  , lookupAgenda
  , baseAgenda
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Cards
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait (Trait)
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
  , QuietHalls' quietHalls
  , DeadOfNight' deadOfNight
  , TheBeastUnleashed' theBeastUnleashed
  , TheCloverClub' theCloverClub
  , UndergroundMuscle' undergroundMuscle
  , ChaosInTheCloverClub' chaosInTheCloverClub
  , RestrictedAccess' restrictedAccess
  , ShadowsDeepen' shadowsDeepen
  , InEveryShadow' inEveryShadow
  , ReturnToPredatorOrPrey' returnToPredatorOrPrey
  , ACreatureOfTheBayou' aCreatureOfTheBayou
  , TheRougarouFeeds' theRougarouFeeds
  , TheCurseSpreads' theCurseSpreads
  ]

instance HasCount DoomCount env Agenda where
  getCount = getCount . agendaAttrs

instance HasStep AgendaStep Agenda where
  getStep = getStep . agendaAttrs

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
  | TheCloverClub' TheCloverClub
  | UndergroundMuscle' UndergroundMuscle
  | ChaosInTheCloverClub' ChaosInTheCloverClub
  | RestrictedAccess' RestrictedAccess
  | ShadowsDeepen' ShadowsDeepen
  | InEveryShadow' InEveryShadow
  | ReturnToPredatorOrPrey' ReturnToPredatorOrPrey
  | ACreatureOfTheBayou' ACreatureOfTheBayou
  | TheRougarouFeeds' TheRougarouFeeds
  | TheCurseSpreads' TheCurseSpreads
  | BaseAgenda' BaseAgenda
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Agenda
deriving anyclass instance AgendaRunner env => RunMessage env Agenda
deriving anyclass instance HasSet Trait env EnemyId => HasModifiersFor env Agenda

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  toId = toId . agendaAttrs
  toTarget = toTarget . agendaAttrs
  isTarget = isTarget . agendaAttrs
  toSource = toSource . agendaAttrs
  isSource = isSource . agendaAttrs

newtype BaseAgenda = BaseAgenda Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseAgenda
  :: AgendaId
  -> Text
  -> AgendaSequence
  -> GameValue Int
  -> (Attrs -> Attrs)
  -> Agenda
baseAgenda a b c d f = BaseAgenda' . BaseAgenda . f $ baseAttrs a b c d

instance HasModifiersFor env BaseAgenda where
  getModifiersFor = noModifiersFor

instance HasActions env BaseAgenda where
  getActions iid window (BaseAgenda attrs) = getActions iid window attrs

instance
  ( HasQueue env
  , HasCount DoomCount env ()
  , HasCount PlayerCount env ()
  )
  => RunMessage env BaseAgenda
  where
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
  QuietHalls' attrs -> coerce attrs
  DeadOfNight' attrs -> coerce attrs
  TheBeastUnleashed' attrs -> coerce attrs
  TheCloverClub' attrs -> coerce attrs
  UndergroundMuscle' attrs -> coerce attrs
  ChaosInTheCloverClub' attrs -> coerce attrs
  RestrictedAccess' attrs -> coerce attrs
  ShadowsDeepen' attrs -> coerce attrs
  InEveryShadow' attrs -> coerce attrs
  ReturnToPredatorOrPrey' attrs -> coerce attrs
  ACreatureOfTheBayou' attrs -> coerce attrs
  TheRougarouFeeds' attrs -> coerce attrs
  TheCurseSpreads' attrs -> coerce attrs
  BaseAgenda' attrs -> coerce attrs
