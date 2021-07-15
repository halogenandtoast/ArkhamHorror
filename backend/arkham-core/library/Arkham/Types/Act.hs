module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  ) where

import Arkham.Prelude hiding (fold)

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Cards
import Arkham.Types.Act.Runner
import Arkham.Types.ActId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Name
import Arkham.Types.Trait

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
  | SaracenicScript' SaracenicScript
  | ThePathToTheHill' ThePathToTheHill
  | AscendingTheHillV1' AscendingTheHillV1
  | AscendingTheHillV2' AscendingTheHillV2
  | AscendingTheHillV3' AscendingTheHillV3
  | TheGateOpens' TheGateOpens
  | OutOfThisWorld' OutOfThisWorld
  | IntoTheBeyond' IntoTheBeyond
  | CloseTheRift' CloseTheRift
  | FindingANewWay' FindingANewWay
  | MysteriousGateway' MysteriousGateway
  | FindingLadyEsprit' FindingLadyEsprit
  | HuntingTheRougarou' HuntingTheRougarou
  | TheCarnevaleConspiracy' TheCarnevaleConspiracy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Act
deriving anyclass instance (HasName env LocationId, ActRunner env) => RunMessage env Act

instance HasSet Trait env LocationId => HasModifiersFor env Act where
  getModifiersFor = genericGetModifiersFor

instance HasStep Act ActStep where
  getStep = ask >>= runReaderT getStep . toAttrs

instance Entity Act where
  type EntityId Act = ActId
  type EntityAttrs Act = ActAttrs

instance Named Act where
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
  , SaracenicScript' saracenicScript
  , ThePathToTheHill' thePathToTheHill
  , AscendingTheHillV1' ascendingTheHillV1
  , AscendingTheHillV2' ascendingTheHillV2
  , AscendingTheHillV3' ascendingTheHillV3
  , TheGateOpens' theGateOpens
  , OutOfThisWorld' outOfThisWorld
  , IntoTheBeyond' intoTheBeyond
  , CloseTheRift' closeTheRift
  , FindingANewWay' findingANewWay
  , MysteriousGateway' mysteriousGateway
  , FindingLadyEsprit' findingLadyEsprit
  , HuntingTheRougarou' huntingTheRougarou
  , TheCarnevaleConspiracy' theCarnevaleConspiracy
  ]
