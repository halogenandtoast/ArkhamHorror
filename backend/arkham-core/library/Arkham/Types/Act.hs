{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Cards
import Arkham.Types.Act.Runner
import Data.Coerce

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
  | MysteriousGateway' MysteriousGateway
  | FindingLadyEsprit' FindingLadyEsprit
  | HuntingTheRougarou' HuntingTheRougarou
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Act
deriving anyclass instance ActRunner env => RunMessage env Act

instance IsAdvanceable Act where
  isAdvanceable = actCanAdvance . actAttrs

instance Entity Act where
  type EntityId Act = ActId
  toId = toId . actAttrs
  toSource = toSource . actAttrs
  toTarget = toTarget . actAttrs
  isSource = isSource . actAttrs
  isTarget = isTarget . actAttrs

lookupAct :: ActId -> Act
lookupAct actId =
  fromJustNote ("Unknown act: " <> show actId) $ lookup actId allActs

allActs :: HashMap ActId Act
allActs = mapFromList $ map
  (toFst $ actId . actAttrs)
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
  , MysteriousGateway' mysteriousGateway
  , FindingLadyEsprit' findingLadyEsprit
  , HuntingTheRougarou' huntingTheRougarou
  ]

actAttrs :: Act -> Attrs
actAttrs = \case
  Trapped' attrs -> coerce attrs
  TheBarrier' attrs -> coerce attrs
  WhatHaveYouDone' attrs -> coerce attrs
  UncoveringTheConspiracy' attrs -> coerce attrs
  InvestigatingTheTrail' attrs -> coerce attrs
  IntoTheDarkness' attrs -> coerce attrs
  DisruptingTheRitual' attrs -> coerce attrs
  AfterHours' attrs -> coerce attrs
  RicesWhereabouts' attrs -> coerce attrs
  CampusSafety' attrs -> coerce attrs
  BeginnersLuck' attrs -> coerce attrs
  SkinGame' attrs -> coerce attrs
  AllIn' attrs -> coerce attrs
  Fold' attrs -> coerce attrs
  MysteriousGateway' attrs -> coerce attrs
  FindingLadyEsprit' attrs -> coerce attrs
  HuntingTheRougarou' attrs -> coerce attrs
