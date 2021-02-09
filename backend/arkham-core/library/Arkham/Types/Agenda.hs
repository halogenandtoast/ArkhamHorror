module Arkham.Types.Agenda
  ( module Arkham.Types.Agenda
  ) where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Cards
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait (Trait)

lookupAgenda :: AgendaId -> Agenda
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId)
    $ lookup agendaId allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = mapFrom
  toId
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
  , ATearInReality' aTearInReality
  , TheMawWidens' theMawWidens
  , RollingBackwards' rollingBackwards
  , DrawnIn' drawnIn
  , OutOfTime' outOfTime
  , StrangeDisappearances' strangeDisappearances
  , TheOldOnesHunger' theOldOnesHunger
  , FeedTheBeast' feedTheBeast
  , ReturnToPredatorOrPrey' returnToPredatorOrPrey
  , ACreatureOfTheBayou' aCreatureOfTheBayou
  , TheRougarouFeeds' theRougarouFeeds
  , TheCurseSpreads' theCurseSpreads
  ]

instance HasList UnderneathCard env Agenda where
  getList = getList . toAttrs

instance HasCount DoomCount env Agenda where
  getCount = getCount . toAttrs

instance HasStep AgendaStep Agenda where
  getStep = getStep . toAttrs

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
  | ATearInReality' ATearInReality
  | TheMawWidens' TheMawWidens
  | RollingBackwards' RollingBackwards
  | DrawnIn' DrawnIn
  | OutOfTime' OutOfTime
  | StrangeDisappearances' StrangeDisappearances
  | TheOldOnesHunger' TheOldOnesHunger
  | FeedTheBeast' FeedTheBeast
  | ReturnToPredatorOrPrey' ReturnToPredatorOrPrey
  | ACreatureOfTheBayou' ACreatureOfTheBayou
  | TheRougarouFeeds' TheRougarouFeeds
  | TheCurseSpreads' TheCurseSpreads
  | BaseAgenda' BaseAgenda
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Agenda
deriving anyclass instance AgendaRunner env => RunMessage env Agenda
deriving anyclass instance HasSet Trait env EnemyId => HasModifiersFor env Agenda

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  type EntityAttrs Agenda = AgendaAttrs

instance NamedEntity Agenda where
  toName = toName . toAttrs

instance TargetEntity Agenda where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Agenda where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

newtype BaseAgenda = BaseAgenda AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseAgenda
  :: AgendaId
  -> Text
  -> AgendaSequence
  -> GameValue Int
  -> (AgendaAttrs -> AgendaAttrs)
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
  , HasId LeadInvestigatorId env ()
  )
  => RunMessage env BaseAgenda
  where
  runMessage msg (BaseAgenda attrs) = BaseAgenda <$> runMessage msg attrs
