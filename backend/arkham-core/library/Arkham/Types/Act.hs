module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window
 hiding (fold)

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Cards
import Arkham.Types.Act.Runner

data Act
  = Trapped' Trapped
  | TheBarrier' TheBarrier
  | WhatHaveYouDone' WhatHaveYouDone
  | UncoveringTheConspiracy' UncoveringTheConspiracy
  | InvestigatingTheTrail' InvestigatingTheTrail
  | IntoTheDarkness' IntoTheDarkness
  | DisruptingTheRitual' DisruptingTheRitual
  | AfterHours' AfterHours
  | RicesWhereabouts' RicesWhereabouts
  | CampusSafety' CampusSafety
  | BeginnersLuck' BeginnersLuck
  | SkinGame' SkinGame
  | AllIn' AllIn
  | Fold' Fold
  | FindingAWayInside' FindingAWayInside
  | NightAtTheMuseum' NightAtTheMuseum
  | BreakingAndEntering' BreakingAndEntering
  | SearchingForTheTome' SearchingForTheTome
  | Run' Run
  | GetTheEngineRunning' GetTheEngineRunning
  | SearchingForAnswers' SearchingForAnswers
  | TheChamberOfTheBeast' TheChamberOfTheBeast
  | MysteriousGateway' MysteriousGateway
  | FindingLadyEsprit' FindingLadyEsprit
  | HuntingTheRougarou' HuntingTheRougarou
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Act
deriving anyclass instance ActRunner env => RunMessage env Act

instance HasStep ActStep Act where
  getStep = getStep . toAttrs

instance Entity Act where
  type EntityId Act = ActId
  type EntityAttrs Act = ActAttrs

instance NamedEntity Act where
  toName = toName . toAttrs

instance TargetEntity Act where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Act where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupAct :: ActId -> Act
lookupAct actId =
  fromJustNote ("Unknown act: " <> show actId) $ lookup actId allActs

allActs :: HashMap ActId Act
allActs = mapFrom
  toId
  [ Trapped' trapped
  , TheBarrier' theBarrier
  , WhatHaveYouDone' whatHaveYouDone
  , UncoveringTheConspiracy' uncoveringTheConspiracy
  , InvestigatingTheTrail' investigatingTheTrail
  , IntoTheDarkness' intoTheDarkness
  , DisruptingTheRitual' disruptingTheRitual
  , AfterHours' afterHours
  , RicesWhereabouts' ricesWhereabouts
  , CampusSafety' campusSafety
  , BeginnersLuck' beginnersLuck
  , SkinGame' skinGame
  , AllIn' allIn
  , Fold' fold
  , FindingAWayInside' findingAWayInside
  , NightAtTheMuseum' nightAtTheMuseum
  , BreakingAndEntering' breakingAndEntering
  , SearchingForTheTome' searchingForTheTome
  , Run' run
  , GetTheEngineRunning' getTheEngineRunning
  , SearchingForAnswers' searchingForAnswers
  , TheChamberOfTheBeast' theChamberOfTheBeast
  , MysteriousGateway' mysteriousGateway
  , FindingLadyEsprit' findingLadyEsprit
  , HuntingTheRougarou' huntingTheRougarou
  ]
